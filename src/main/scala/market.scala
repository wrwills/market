/**
 * mock FX aggregator
 */
import scala.concurrent.duration._
import akka.actor._
import org.joda.time.DateTime
import scalaz._

trait MarketSimulationActorsContainer { 
  import FXAggregator._

  val actorSystem = ActorSystem("market-sim")
  val scheduler = actorSystem.scheduler
  import scheduler.{ schedule, scheduleOnce }
  import actorSystem.dispatcher
  
  class SpotRatesActor extends Actor { 
    var ladder: CurrencyLadder = Map()
    def receive = { 
      case x: SpotRates => { 
        ladder = updateLadder(x,ladder)
      }
      case 'log => println(output( DateTime.now, ladder))
    }
  }

  /*
  class AggregateLogger extends Actor { 
    
  }*/

  /**
   * read csv file; parse lines and send out to the spot rates actor
   * try as far as possible to send out at accurate time since start of simulation
   * csv file contains floating point timestamps but scheduling uses milliseconds; my
   * understanding is that the jvm doesn't support fine-grained scheduling in nanoseconds
   * would you need a hard real time system for that?
   */
  class MarketSimulatorFeed extends Actor { 
    var startTime: Long = 0
    var pendingTs: Double = 0.0
    var pending: List[SpotRates] = List()
    var next: Option[(Double,SpotRates)] = None
    
    def fill(pTs: Double, accum: List[SpotRates]): (Double, List[SpotRates], Double, SpotRates)  = { 
      // might be better to use a stream here
      val (ts,sr) = parseCsvSpotRate(csvIterator.next.toList).get
      if (accum.isEmpty) 
        fill(ts, List(sr))
      else if (ts == pTs)
        fill(pTs, sr :: accum)
      else
        (pTs, accum, ts, sr) 
    }

    def receive = { 
      case 'start => { 
        startTime =  DateTime.now.getMillis // System.nanoTime
        self ! 'next
      }
      case 'next => { 
        pending.reverse.foreach( spotRatesActor ! _ )
        val (pts, nextPending, afterNextTs, afterNextSpotrates) = 
          if (next.isDefined)
            fill(next.get._1, List(next.get._2))
          else
            fill(0.0, List())
        pending = nextPending
        pendingTs = pts
        next = Some(afterNextTs, afterNextSpotrates)
        scheduleOnce( 
          (pendingTs.toLong - (DateTime.now.getMillis - startTime)).milliseconds,
          self,
          'next
          )
      }
    }
  }

  val csvIterator: scala.collection.Iterator[Seq[String]]
  // val csvStream: Stream[List[String]]

  val marketSimulatorFeed = actorSystem.actorOf(Props[MarketSimulatorFeed]) //;,"simulated-feed")
  val spotRatesActor = actorSystem.actorOf(Props[SpotRatesActor]) // ,"spot-rates")

  val logInterval = 100 millis

  def startSimulation = { 
    marketSimulatorFeed ! 'start
    schedule( logInterval, logInterval, spotRatesActor, 'log)
  }
}

object FXAggregator { 
  import Scalaz._

  //val duration = 100 millis
  
  type Market = String
  type CurrencyPair = String

  case class Rate( amt: Double, size: Int) { 
    def output = size + " @ " + amt
  }

  //case class SpotRates(ts: Double, market: Market, currencyPair: CurrencyPair,  bid: Rate, ask: Rate)
 
  // perhaps it would be better to just use an hlist for this?
  case class SpotRates(market: Market, currencyPair: CurrencyPair,  bid: Rate, ask: Rate)

  type CurrencyLadder = Map[(Market,CurrencyPair), (Rate,Rate)]

  def updateLadder(newRate: SpotRates, oldLadder: CurrencyLadder): CurrencyLadder =
    oldLadder + ((newRate.market, newRate.currencyPair) -> (newRate.bid, newRate.ask))
  
  val dateFormat = org.joda.time.format.DateTimeFormat.forPattern("yyyyMMdd")

  // An example of a line of output where n=3:
  // 11/03/2008 11:24:27,100 GBPUSD - Bid: 10 @ 1.6831, 5 @ 1.6832, 30 @ 1.6835 / Ask: 27 @ 1.6837, 19 @ 1.6838, 4 @ 1.6841
  def output(ts: DateTime, ladder: CurrencyLadder, n: Int = 3) = {     
    val pairs = ladder.keys.map(_._2)
    pairs.map( (x:CurrencyPair) => { 
      val rates = ladder.filter( _._1._2 == x).map( _._2 ).toSeq
      ts + " " + x + " - Bid: " + 
      rates.map( _._1 ).sortBy( _.amt ).reverse.take(n).sortBy( _.amt ).map( _.output ).mkString(" ") + " / Ask: " + 
      rates.map( _._2 ).sortBy( _.amt ).take(n).map( _.output ).mkString(" ")      
    })
  }

  def parseCsvSpotRate(xs: List[String]) = {  // : Option[SpotRates] = { 
    val ts :: mkt :: cpair :: bid :: bidsize :: ask :: asksize :: Nil = xs
    val bidR = (bid.parseDouble.toOption |@| bidsize.parseInt.toOption) { Rate(_,_) }
    val askR = (ask.parseDouble.toOption |@| asksize.parseInt.toOption) { Rate(_,_) }
    (ts.parseDouble.toOption |@| bidR |@| askR) { (x,y,z) =>  (x, SpotRates( mkt, cpair, y, z)) }
    // (ts.parseDouble.toOption |@| bidR |@| askR) {  (_, SpotRates( mkt, cpair, _, _)) }
  }

}

object MarketSimulation extends App with MarketSimulationActorsContainer { 
  import com.github.tototoshi.csv.CSVReader
  val reader = CSVReader.open("market.csv")
  val csvIterator = reader.iterator
  //val csvStream = reader.toStream
  // ignore header line
  csvIterator.next()

  startSimulation
}


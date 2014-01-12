/**
 * mock FX aggregator
 */
import scala.concurrent.duration._
import akka.actor.Actor
import org.joda.time.DateTime

object FXAggregator { 

  val duration = 100 millis
  
  type Market = String
  type CurrencyPair = String

  case class Rate( amt: Double, size: Int) { 
    def output = size + " @ " + amt
  }

  //case class SpotRates(ts: Double, market: Market, currencyPair: CurrencyPair,  bid: Rate, ask: Rate)
 
  case class SpotRates(market: Market, currencyPair: CurrencyPair,  bid: Rate, ask: Rate)

  type CurrencyLadder = Map[(Market,CurrencyPair), (Rate,Rate)]

  def updateLadder(newRate: SpotRates, oldLadder: CurrencyLadder): CurrencyLadder =
    oldLadder + ((newRate.market, newRate.currencyPair) -> (newRate.bid, newRate.ask))
  
  def output(ts: DateTime, ladder: CurrencyLadder, n: Int = 3) = {     
    val pairs = ladder.keys.map(_._2)
    pairs.map( (x:CurrencyPair) => { 
      val rates = ladder.filter( _._1._2 == x).map( _._2 ).toSeq
      ts + " " + x + " - Bid:" + 
      rates.map( _._1 ).sortBy( _.amt ).reverse.take(n).sortBy( _.amt ).map( _.output ).mkString(" ") + "Ask: " + 
      rates.map( _._2 ).sortBy( _.amt ).take(n).map( _.output ).mkString(" ")      
    })
  }
    
    

}


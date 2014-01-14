//Implement the missing methods.
object CountCharacters {
  import Math.pow
 
  def digitsToString(x: Char): String = digitCharToString(x)

  def digitCharToString(x: Char) = x match { 
    case '0' => ""
    case '1' => "one"
    case '2' => "two" 
    case '3' => "three"
    case '4' => "four"
    case '5' => "five"
    case '6' => "six"
    case '7' => "seven"
    case '8' => "eight"
    case '9' => "nine"
  }

  //def twoDigitsToString(x: (Char, Char)) = x match { 
  def digitsToString(x: (Char, Char)): String = x match { 
    case ('1','0') => "ten"
    case ('1','1') => "eleven"
    case ('1','2') => "twelve"
    case ('1','3') => "thirteen"
    //case ('1','8') => "eighteen"
    case _ => { 
      val d = digitCharToString(x._2)
      if (x._1 == '1') 
        d + "teen" 
      else if (x._1 == '0')
        d
      else 
        digitCharToString(x._1).replace("four","for").replace("two","twen").replace("three","thir").replace("five","fif") + "ty " + d
    }
  }
  
  def multipleOfThreeDigitsToString(i: Int) = i match { 
    case 1 => "thousand"
    case 2 => "million"
    case 3 => "billion"
    case 4 => "trillion"
  }

  def digitsToString(x: List[Char]): String = x match {
    case Nil => ""
    case a :: Nil => digitCharToString(a)
    case a :: b :: Nil => digitsToString((a,b))
    case a :: b :: c :: Nil => { 
      val h = digitCharToString(a)
      val start = if (h.isEmpty) "" else h + " hundred " 
      start + digitsToString( (b,c) )
    }
    case _ => { 
      val splitPt = { 
        val mod = x.length % 3
        if (mod == 0) 3 else mod
      }
      val (a,b) = x.splitAt( splitPt )
      println( a + " " + b)
      digitsToString(a) + " " + multipleOfThreeDigitsToString(b.length / 3) + " " + digitsToString(b)
    }
  }
    

  /*
    returns i as spelled in english (without commas, "and"s etc)
    assume US notation, ie billion = 10^9
    eg.
     toWords(9) = "nine"
     toWords(99) = "ninety nine"
     toWords(999) = "nine hundred ninety nine"
  */
  def toWords(i: Int): String =  { 
    val chars = String.valueOf(i).toCharArray.toList
    digitsToString(chars).replace("tt", "t") // for 'eighteen' and 'eighty'    
  }
 
  //countCharsInWords(9) = 4
  //countCharsInWords(99) = 10
  //countCharsInWords(999) = 21
  def countCharsInWords(i: Int): Int = toWords(i).filter(_ != ' ').length

  def countCharsExtraCount(l: Int): Int = { 
    val extraCounts = 
      List((2,7), (3,8), (6,7), (9,7), (12,8)).map( x => (pow(10,x._1), x._2) )
    extraCounts.takeWhile( l > _._1 ).map( _._2).sum
  }

  def countCharsInDigit(i: Int): Int = { 
    require( i < 10 )
    if ( i == 0) 
      0
    else if (i == 1 | i == 2 | i == 6)
      3
    else if (i == 4 | i == 5 | i == 9)
      4
    else
      5
  }

  def countCharsLessThanHundred(i: Int) = { 
    require( i < 100 )
    val tens = i / 10
    val digit = i % 10  
    val digitCount = countCharsInDigit(digit)
    tens match { 
      case 0 => digitCount
      case 1 => 
        digitCount + (if (digit == 4 | digit == 6 | digit == 7 | digit == 9) 4 else 3)
      case _ =>
        digitCount + 
        countCharsInDigit(tens) + 
        (if (tens == 2 | tens == 3 | tens == 4 | tens == 5) 1 else 2)                
    }
  }

  def countCharsInWordsO(i: Int): Int = 
    if (i < 1000)
      countCharsInDigit( i / 100 ) +  countCharsLessThanHundred( i % 100 )     
    else
      countCharsInWordsO(i / 1000) + countCharsInWordsO(i % 1000)
  

  /*
    a more efficient implementation of countCharsInWords.
    This does not need to re-use the above and may be an entirely different algorithm.
    
  * This implementation proceeds by directly adding up the count of the letters for the
  * words in the number.  This should be slightly quicker and is more direct
  * It also allows us to do away with using String.valueOf -- a method which I suspect might
  * take up the most amount of time in this computation
  */
  def countCharsInWordsOptimised(i: Int): Int = 
    countCharsInWordsO(i) + countCharsExtraCount(i)
    


  /* tests
   CountCharacters.toWords(10)
CountCharacters.toWords(999)
CountCharacters.toWords(99)
CountCharacters.toWords(1099)
CountCharacters.toWords(1099)
CountCharacters.toWords(99)
CountCharacters.toWords(999)
CountCharacters.toWords(1099)
CountCharacters.toWords(1799)
CountCharacters.toWords(101799)
CountCharacters.toWords(111799)
CountCharacters.toWords(2111799)
CountCharacters.toWords(25)
*/
  
}

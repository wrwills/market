//Implement the missing methods.
object CountCharacters {
 
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
    case _ => { 
      val d = digitCharToString(x._2)
      if (x._1 == '1') 
        d + "teen" 
      else if (x._1 == '0')
        d
      else 
        digitCharToString(x._1) + "ty " + d
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
    digitsToString(chars)    
  }
 
  //countCharsInWords(9) = 4
  //countCharsInWords(99) = 10
  //countCharsInWords(999) = 21
  def countCharsInWords(i: Int): Int = toWords(i).filter(_ != ' ').length
 
  /*
    a more efficient implementation of countCharsInWords.
    This does not need to re-use the above and may be an entirely different algorithm.
  */
  def countCharsInWordsOptimised(i: Int): Int = error("not yet implemented")
 
}

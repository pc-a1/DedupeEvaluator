/*
 * Copyright, 2012, AgilOne LLC 
 * All Rights Reserved 
 * Company Confidential
 */
package dedoop

/**
 * @author erisa
 * Algorithm used for string comparison. Classes that extend this trait implement their own comparison algorithm.
 */
trait Algorithm {
	def compare(firstWord : String, secondWord : String) : Double 
}

object Algorithm {
  
	object Type extends Enumeration {
		val Levenshtein= "Levenshtein"
		val JaroWinkler = "Jaro-Winkler"
	}
	
  def apply(object_name: String): Algorithm = (object_name) match{
    case (Type.Levenshtein) => return LevenshteinDistance
    case (Type.JaroWinkler) => return JaroWinklerDistance
    case _ => throw new IllegalArgumentException
  }
}
/*
 * Copyright, 2012, AgilOne LLC 
 * All Rights Reserved 
 * Company Confidential
 */
package dedoop
import scala.math.min
import scala.math.max
/**
 * @author erisa
 */
object LevenshteinDistance extends Algorithm {

	/**
	 * Implementation of the Levenshtein algorithm which computes the distance. 
	 * (how alike or different two strings of symbols are) between two Strings. 
	 * @param s1 First string to be compared.
	 * @param s2 Second string to be compared.
	 * @return An integer that represents the distance between the two strings. Equal strings have a Levenshtein value of 0. 
	 * This scala version is a better version than the java one (in terms of memory), because it uses 2 one-dimentional arrays, instead of a two-dimentional array. 
	 */
	
	 private def stringDistance(s1: String, s2: String): Int = {
	  def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)
	
	  var dist = ( new Array[Int](s1.length + 1),
	               new Array[Int](s1.length + 1) )
	
	  for (i <- 0 to s1.length) dist._2(i) = i
	  for (j <- 1 to s2.length) {
	    val (newDist, oldDist) = dist
	    newDist(0) = j
	    for (i <- 1 to s1.length) {
	      newDist(i) = minimum (
	        oldDist(i) + 1,
	        newDist(i-1) + 1,
	        oldDist(i-1) + (if (s1(i-1) == s2(j-1)) 0 else 1)
	      )
	    }
	    dist = dist.swap
	  }
	
	  dist._2(s1.length)
	}
	
	 
  /**
   * This method calculates Levenshtein Distance and returns a double [0, 1] which is a measure 
   * of similarity between two given strings; 1 being exact match.
   * @param firstString A string which is the first value to be compared. This string may be empty.
   * @param secondString A string which is the second value to be compared. This string may be empty.
   * @return A double which is a measure of similarity between firstString and secondString.
   */
	def compare(firstString: String, secondString: String): Double = {
		if (firstString == secondString) 1
		else 1-stringDistance(firstString, secondString).toDouble/(max(firstString.length, secondString.length))
	}
}
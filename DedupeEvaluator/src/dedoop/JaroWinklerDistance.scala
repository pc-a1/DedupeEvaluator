/*
 * Copyright, 2012, AgilOne LLC 
 * All Rights Reserved 
 * Company Confidential
 */
package dedoop
import scala.math.min
import scala.math.max
/**
 * String comparison using Jaro-Winkler Distance algorithm.
 * Wikipedia link: http://en.wikipedia.org/wiki/Jaro�Winkler_distance
 * @author erisa
 *
 */
object JaroWinklerDistance extends Algorithm {
  
 
  private val DefaultMismatchScore = 0.0
  private val MinPrefixTestLength = 4
  private val PrefixAdustmentScale = 0.1F

	/**
	 * Returns a string buffer of characters from firstString within secondString if they are of a given
	 * distance separation from the position in firstString.
	 * @param firstString A string which is the first value to be compared. It may be an empty string.
	 * @param secondString A string which is the second value to be compared. It may be an empty string.
	 * @param distanceSep The distance separation.
	 * @return A string buffer of characters from firstString within secondString if they are of a given
	 *         distance separation from the position in firstString.
	 */
	private def getCommonCharacters(firstString: String, secondString: String, distanceSep: Int): StringBuilder = {
		var firstStringLength = firstString.length()
		if (firstStringLength == 0) {
			return null
		}
		var secondStringLength = secondString.length()
		if (secondStringLength == 0) {
			return null
		}
		val returnCommons = new StringBuilder()
		val copy = new StringBuilder(secondString)
		var i = 0
		for (firstStringChar <- firstString) {
			var foundIt = false
			var j = max(0, i - distanceSep)
			while (!foundIt && j < min(i + distanceSep, secondStringLength)) {
				if (copy.charAt(j) == firstStringChar) {
					foundIt = true
					returnCommons.append(firstStringChar)
					copy.setCharAt(j, '#')
				}
				j += 1
			}
			i += 1
		}
		returnCommons
	}

	/**
	 * Returns a double representing the jaro distance that we need for calculating Jaro Winkler Distance.
	 * @param firstString A string which is the first value to be compared. It may be an empty string.
	 * @param secondString A string which is the second value to be compared. It may be an empty string.
	 * @return A double which represents jaro distance.
	 */
	private def getSimilarity(firstString: String, secondString: String): Double = {

		//get half the length of the string rounded up - (this is the distance used for acceptable transpositions)
		val halflen: Int = ((min(firstString.length, secondString.length)) / 2) + 1

		//get common characters
		val common1 = getCommonCharacters(firstString, secondString, halflen)
		val common2 = getCommonCharacters(secondString, firstString, halflen)

		//check for zero in common
		if (common1 == null || common1.length == 0) {
			return DefaultMismatchScore
		}
		val commonMatches = common1.length
		// check for same length common strings returning 0.0f if not the same
		if (commonMatches != common2.length) {
			return DefaultMismatchScore;
		}

		//get the number of transpositions
		var transpositions: Int = 0

		for (i <- 0 to commonMatches - 1) {
			if (common1.charAt(i) != common2.charAt(i)) {
				transpositions += 1
			}
		}

		transpositions = transpositions / 2

		//calculate jaro distance
		(commonMatches / (3.0 * firstString.length.toDouble) + commonMatches / (3.0 * secondString.length.toDouble) + (commonMatches - transpositions) / (3.0 * commonMatches.toDouble))

	}

	/**
	 * This method calculates PrefixLength which is a metric we need for calculating Jaro-Winkler Distance later on.
	 * @param firstString A string which is the first value to be compared. It may be an empty string.
	 * @param secondString A string which is the second value to be compared. It may be an empty string.
	 * @return int An integer which is a metric we need for calculating Jaro-Winkler Distance later on.
	 */
	private def getPrefixLength(firstString: String, secondString: String): Int = {
		val n = min(MinPrefixTestLength, min(firstString.length(), secondString.length()))
		for (i <- 0 until n) {
			if (firstString.charAt(i) != secondString.charAt(i)) {
				return i
			}
		}
		n
	}

	/**
	 * This method calculates Jaro-Winkler Distance and returns a double [0, 1] which is a measure
	 * of similarity between two given strings; 1 being exact match.
	 * @param firstString A string which is the first value to be compared. This string may be empty.
	 * @param secondString A string which is the second value to be compared. This string may be empty.
	 * @return A double which is a measure of similarity between firstString and secondString.
	 */
	def compare(firstString: String, secondString: String): Double = {
		if (firstString == secondString) 1
		else {
			val dist = getSimilarity(firstString, secondString)
			val prefixLength = getPrefixLength(firstString, secondString)
			dist + (prefixLength * PrefixAdustmentScale * (1.0 - dist))
		}
	}

}
  
  
  
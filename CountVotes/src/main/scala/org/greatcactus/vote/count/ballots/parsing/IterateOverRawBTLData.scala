/*
    Copyright 2015-2018 Silicon Econometrics Pty. Ltd.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

 */

package org.greatcactus.vote.count.ballots.parsing

import org.greatcactus.vote.count.ballots.ElectionMetadata

/**
  * Iterate over raw, possibly informal, below the line data.
  */
trait IterateOverRawBTLData {
  def meta : ElectionMetadata
  /** Iterate over a set of preferences, being the mark next to the candidates in candidate order. */
  def foreach(f:Array[String]=>Unit)
  /** Number of BTL votes processed. */
  def rowsSoFar : Int
  /** Stuff about the current row being iterated over, such as polling station */
  def currentRowMetadata : Map[String,String]
}


/** Search for repeated numbers and missing numbers in a preference list, and produce a histogram by number */
object ObviousErrorsInBTLVotes {
  def search(over:IterateOverRawBTLData): ObviousErrorsInBTLVotes = {
    val numCandidates = over.meta.numCandidates
    val repeated = new Array[Int](numCandidates)
    val repeatedPapers = new Array[Int](numCandidates)
    val missing = new Array[Int](numCandidates)
    for (btl <- over) {
      val found = new Array[Int](numCandidates)
      for (v <- btl) if (!v.isEmpty) {
        val n = (if (v == "*" || v == "/") 1 else v.toInt) -1 // make 0 based.
        if (n >= 0 && n < found.length) {
          found(n)+=1
          if (found(n)>1) {
            repeated(n) += 1
            if (found(n)==2) repeatedPapers(n)+=1
          }
        }
      }
      for (i <- 0 until found.length - 1) if ((i==0 || found(i-1)>0) && (found(i + 1)>0) && (found(i)==0)) missing(i) += 1
    }
    new ObviousErrorsInBTLVotes(over.rowsSoFar,repeated,repeatedPapers,missing)
  }
}

/** repeated[i] is the number of times the preference i+1 is present. repeatedPaper[i] is the number of papers that have at least one repeated i. Missing means that the prior (unless 1) and subsequent numbers are present. */
class ObviousErrorsInBTLVotes(val numVotes:Int,val repeated:Array[Int],val repeatedPapers:Array[Int],val missing:Array[Int])


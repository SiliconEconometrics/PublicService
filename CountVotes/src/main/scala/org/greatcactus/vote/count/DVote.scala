/*
    Copyright 2016-2017 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count

import java.io.PrintWriter
import java.io.FileWriter

/** 
 * A partially distributed vote. Ignore votes before upto. Really the most important data structure in the program.
 * prefs[i] is the candidate given the i-th preference (starting counting from zero).
 * numVoters is the number who cast this vote (identical votes are conflated).
 * upto indicates which preference we are up to on this vote given its past distributions. 
 * src is where this vote came from (SATL,RATL,BTL, etc)
 **/
class DVote(val upto:Int,val numVoters:Double,val prefs:Array[Int],val src:VoteSource) {
  def current : Int = prefs(upto)
  def next : DVote = new DVote(upto+1,numVoters,prefs,src)
  def isExhausted : Boolean = upto==prefs.length
  def skipNotContinuingCandidates(continuingCandidates:Set[Int]) : DVote = {
    if (isExhausted || continuingCandidates(current)) this
    else next.skipNotContinuingCandidates(continuingCandidates)
  }
  def applyTransferValue(transferValue:Double) : DVote = new DVote(upto,numVoters*transferValue,prefs,src)
  override def toString = numVoters.toString+"* "+prefs.drop(upto).mkString(",")
  def toMichelleFormat = "("+prefs.mkString(",")+"):"+numVoters.toInt
}

object DVote {
  def saveMichelleFormat(file:java.io.File,votes:Seq[DVote]) {
    file.getParentFile().mkdirs()
    val pw = new PrintWriter(new FileWriter(file))
    for (v<-votes) pw.println(v.toMichelleFormat)
    pw.close()
  }
}

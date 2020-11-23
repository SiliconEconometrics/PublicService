/*
    Copyright 2019 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count.report

import java.io.{File, FileWriter, PrintWriter}

import org.greatcactus.vote.count.ElectionResultReport
import org.greatcactus.vote.count.MainDataTypes.{CandidateIndex, Tally}

import scala.collection.mutable.ArrayBuffer

class OutputsFromHistory {

}


/** A format requested by Michelle  */
object MichelleSTVOutputFormat {
  def print(f:File,report:ElectionResultReport) : Unit = {
    val pw = new PrintWriter(new FileWriter(f))
    try {
      print(pw,report)
    } finally { pw.close() }
  }
  def print(pw:PrintWriter,report:ElectionResultReport): Unit = {
    // Candidates listed in order of elimination/election (0 - first candidate in candidate list, and so on)
    // eg. 0,54,6,2,3,8,...
    val candidatesInOrderOfEliminationOrElection = new ArrayBuffer[(CandidateIndex,Tally)]
    for (h<-report.history) {
      val happensHere = h.countType.candidatesEliminated.reverse.map{c=>(c,h.totalAtStart(c).toLong)}++h.electedCandidates.map{_._1}.map{c=>(c,h.totalAtEnd(c).toLong)}
      candidatesInOrderOfEliminationOrElection++=happensHere.filter{case (c,_) => candidatesInOrderOfEliminationOrElection.forall{_._1 !=c}}
    }
    pw.println("# Candidates in order of elimination or election")
    pw.println(candidatesInOrderOfEliminationOrElection.map{_._1}.mkString(","))
    // Whether each candidate got a seat or not (0 - eliminated/no seat, 1 - given a seat):
    // eg. 1,1,0,0,0,0,...,1,1,0,0,0,0,0
    val whetherCandidateGotASeat = for (c<-report.candidates.indices) yield if (report.electedCandidates.contains(c)) 1 else 0
    pw.println("# Whether each candidate got a seat 1=yes, by candidate id")
    pw.println(whetherCandidateGotASeat.mkString(","))

    // Then for each candidate their tally upon election or elimination: one line for each candidate
    //
    // 0,45873
    // 1,500
    // 2,5000
    //for (h<-report.history; w<-h.electedCandidates) pw.println(w._1.toString+","+h.string_totalAtEnd(w._1))
    pw.println("# For each candidate, their tally upon election or elimination; one line for each candidate")
    for ((candidate,tally)<-candidatesInOrderOfEliminationOrElection) pw.println(candidate.toString+","+tally.toString)
  }
}
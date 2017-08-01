/*
    Copyright 2015-2017 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count.federal

import org.greatcactus.vote.count._
import org.greatcactus.vote.count.MainDataTypes._

import java.io.File

/** Compute a histogram of all the repeated numbers in a given person's vote (errors) */
object HistogramOfRepeatedNumbers extends App {
  val sourcedir = new File("Elections/Federal/2016")
    
  def analyse(state:String) {
    println("State : "+state)
    val zipfile = new File(sourcedir,"aec-senate-formalpreferences-20499-"+state+".zip")
    val candidateInfoGenerator = new AECCandidateInformationSource2016FirstPrefsByStateByVoteType(new File(sourcedir,"SenateFirstPrefsByStateByVoteTypeDownload-20499.csv"))
    candidateInfoGenerator.read(state)
    val candidates = new AECCandidateInformation(candidateInfoGenerator)
    val numGroups = candidates.rawgroups.length-(if (candidates.rawgroups.last=="UG") 1 else 0) // can't do an ATL vote for UG.
    val numCandidates = candidates.orderedCandidates.length
    val repeated = new Histogram(numCandidates)
    val missing = new Histogram(numCandidates)
    val splitter = new Splitter

    for (cols<-CSVHelper.fromZipFile(zipfile, state+".csv",2)) {
      val prefstr = cols(5) // contains a comma separated string containing written preferences, first for ATL then BTL.
      val votes = splitter.split(prefstr)
      val (atl,btl) = votes.splitAt(numGroups)
      val found = new Array[Boolean](numCandidates)
      for (v<-btl) if (!v.isEmpty) {
        val n = if (v=="*" || v=="/") 1 else v.toInt
        if (n>=0 && n<found.length) {
          if (found(n)) repeated.add(n)
          else found(n)=true
        }
      }
      for (i<-0 until found.length-2) if (found(i) && found(i+2) && ! found(i+1)) missing.add(i+1)
    }
    println("Missing")
    missing.print()
    println("Duplicates")
    repeated.print()
  }
  analyse("VIC")
}

class Histogram(len:Int) {
  private val a = new Array[Int](len)
  private var largest = 0
  def add(n:Int) {
    if (largest<n) largest=n
    if (n<a.length && n>=0) a(n)+=1
  }
  def print() {
    for (i<-1 to (largest min (a.length-1))) println(i.toString+"\t"+a(i)) 
  }
}
/*
    Copyright 2015-2016 Silicon Econometrics Pty. Ltd.

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

import java.io.BufferedReader
import java.io.FileReader
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.ListBuffer
import java.io.FileOutputStream
import java.io.FileInputStream
import java.io.PrintWriter
import java.io.FileWriter


sealed class ElectionData(
    val name : String,
    val candidates : Array[Candidate],
    val satls : Array[SATL],
    val ratls : Array[ATL],
    val btls : Array[BTL]
    ) {
  def numSATLs = satls.map{_.numVoters}.sum
  def numRATLs = ratls.map{_.numVoters}.sum
  def printStatus() {
    println("SATLs : "+numSATLs)
    println("RATLS : "+numRATLs+" num distinct = "+ratls.length)
    println("BTLs : "+btls.length)
    println("Total formal votes : "+totalFormalVotes)
    println("Candidates : " + candidates.size)
  }
  def totalFormalVotes = numSATLs+numRATLs+btls.length
  def makeVotes : Array[Vote] = {
    val res = new scala.collection.mutable.ArrayBuffer[Vote]
    val groups : Map[String,Array[Int]] = Map.empty++ (for ((group,l)<-candidates.zipWithIndex.groupBy{ _._1.group }) yield (group,l.map{_._2}.toList.sorted.toArray))
    for (s<-satls) res+=new Vote(groups(s.group.toString),s.numVoters)
    for (s<-ratls) res+=new Vote(s.groups.flatMap{c=>groups(c.toString)},s.numVoters)
    for (b<-btls) res+=new Vote(b.candidates,1)
    res.toArray
  }
  def candidateIndex(name:String) : Int = candidates.indexWhere { _.name==name}
  def numCandidates = candidates.length
  
  /** Same data, should produce same results, but with the id numbers of the candidates reversed. Used for testing... if it doesn't produce the same results, something is wrong. */
  def reverseCandidateOrder : ElectionData = {
    new ElectionData(name,candidates.reverse,satls,ratls,btls.map{btl => new BTL(btl.candidates.map{numCandidates-1-_})})
  }
}

trait Dumpable {
  def line:String
}

sealed class Candidate(val name:String,val group:String,val position:Int) extends Dumpable {
  def line = name+"\t"+group+"\t"+position
}
    


class Vote(/** ordered list of candidates */ val preferences:Array[Int],val numVoters:Int)



sealed class SATL(val group:Char,val numVoters:Int) extends Dumpable {
  def line = ""+group+"\t"+numVoters
}

sealed class ATL(/** groups listed in preference order */ val groups:Array[Char],val numVoters:Int) extends Dumpable {
  def line = groups.mkString("")+"\t"+numVoters
} 

sealed class BTL(/** candidate ids listed in preference order */ val candidates:Array[Int]) extends Dumpable {
  def line = candidates.mkString(",")
}




class VoteInterpreter(groups:String,numCandidates:Int,numGroups:Int) {
    val btl = new scala.collection.mutable.HashMap[Int,BelowTheLineBuilder]
    val atl = new scala.collection.mutable.HashMap[Int,AboveTheLineBuilder]
    val satlCounts = new Array[Int](groups.length)
  
    def addBTL(ballotID:Int,candidate:Candidate,preferenceNumber:Int) {
              val v = btl.getOrElseUpdate(ballotID,new BelowTheLineBuilder(ballotID,numCandidates))
              v.addVote(candidate,preferenceNumber)      
    }
    def addRATL(ballotID:Int,groupCode:Char,preferenceNumber:Int) {
              val v = atl.getOrElseUpdate(ballotID,new AboveTheLineBuilder(ballotID,numGroups))
              v.addVote(groupCode,preferenceNumber)      
    }
    def addSATL(groupIndex:Int) {
      satlCounts(groupIndex)+=1
    }
    def getData(orderedCandidates:Array[Candidate],name:String) : ElectionData = {
      //println("SATLs : "+satlCounts.sum)
      //println("RATLs : "+atl.size)
      //println("BTLs : "+btl.size)
      //println("candidates : "+orderedCandidates.length)

      val candidateToIndex : Map[Candidate,Int]=Map.empty++orderedCandidates.zipWithIndex
      val satls = for ((count,index)<-satlCounts.zipWithIndex) yield new SATL(groups(index),count)
      def canonatls(atlmap:scala.collection.mutable.HashMap[Int,AboveTheLineBuilder]) = {
        val strings = (for (v<-atlmap.values) yield v.get).toList
        val bunched = for ((s,l)<-strings.groupBy { a => a }) yield new ATL(s.toCharArray(),l.length)
        bunched.toArray
      }
      val btls = for (v<-btl.values) yield v.get(candidateToIndex)
      new ElectionData(name,orderedCandidates,satls.toArray,canonatls(atl),btls.toArray)
    }
}


class AboveTheLineVote(val ballotID:Int,/** groups voted for, in order */val groups:List[Int])

class AboveTheLineBuilder(val ballotID:Int,numGroups:Int) {
  val prefs:Array[Char]=new Array[Char](numGroups)
  
  def addVote(group:Char,preferenceNumber:Int) { prefs(preferenceNumber-1)=group } // LoadFromFile.groupIndex(group))=preferenceNumber}
  
  def get : String = { 
    prefs.toList.takeWhile(_ !=0).mkString("")
  }
}

class BelowTheLineBuilder(val ballotID:Int,numCandidates:Int) {
  val candidates = new Array[Candidate](numCandidates)
  def addVote(candidate:Candidate,preferenceNumber:Int) { candidates(preferenceNumber-1)=candidate}
  def get(candidateIndex: Candidate=>Int) : BTL = {
    val nonnull = candidates.takeWhile(_ !=null)
    new BTL(nonnull.map(candidateIndex))
  }

}
/*
    Copyright 2015-2019 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count.ballots

import java.io.{BufferedReader, File, FileReader}

import org.greatcactus.vote.count.MainDataTypes.CandidateIndex
import org.greatcactus.vote.count.ballots.GroupInformation.{GroupID, GroupIndex}
import org.greatcactus.vote.count.margin.Margin

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.reflect.ClassTag


/**
  * Information on the ballots and who voted for whom. Ready for an EC to apply some counting algorithm to.
  */

sealed class ElectionData(
    val meta : ElectionMetadata,
    val satls : Array[SATL],
    val ratls : Array[ATL],
    val btls : Array[BTL],
    val numInformal : Int
    ) {
 // def name : String = meta.electionName.name
 // def year : String = meta.electionName.year
  def candidates : Array[Candidate] = meta.candidates
  def groupInfo : Array[GroupInformation] = meta.groupInfo

  // Somewhat obsolete set of functions.
  def groupFromID: Map[GroupID, GroupInformation] = meta.groupFromID
  def groupIndexFromID: Map[GroupID, GroupIndex] = meta.groupIndexFromID
  def groupNameFromID: Map[GroupID, String] = meta.groupNameFromID
  def candidateIndexFromName: Map[String, CandidateIndex] = meta.candidateIndexFromName

  def numSATLs : Int = satls.map{_.numVoters}.sum
  def numRATLs : Int = ratls.map{_.numVoters}.sum
  def numBTLs : Int = btls.map{_.numVoters}.sum
  def printStatus() {
    println(meta.electionName.toString)
    println("SATLs : "+numSATLs)
    println("RATLS : "+numRATLs+" num distinct = "+ratls.length)
    println("BTLs : "+numBTLs+" num distinct = "+btls.length)
    println("Total formal votes : "+totalFormalVotes)
    println("Total informal votes : "+numInformal)
    println("Candidates : " + candidates.length)
    if (consolidatedNumberOfTicketChoiceCombinations>1)  println("Number of ticket start combinations : "+consolidatedNumberOfTicketChoiceCombinations)
  }
  def totalFormalVotes : Int = numSATLs+numRATLs+numBTLs
  val usesGroupVotingTickets : Boolean = groupInfo.exists { _.tickets.length>0 }
  val ticketRoundingChoices : Array[Int] = for (satl<-satls) yield { // number of random choices EC has to make for group voting tickets, by SATL.
    val fromtickets = groupFromID(satl.group).tickets.length max 1 // intrinsic number of choices, but may not crop up
    if (satl.numVoters%fromtickets==0) 1 else fromtickets
  }
  val candidatesInGroup : Map[GroupID,Array[Int]] = Map.empty++ (for ((group,l)<-candidates.zipWithIndex.groupBy{ _._1.group }) yield (group,l.map{_._2}.toList.sorted.toArray))
  private val consolidatedNumberOfTicketChoiceCombinations : Int = ticketRoundingChoices.product
  def makeVotes(ticketRoundingChoicesMade:Map[GroupID,Int]=Map.empty) : Array[Vote] = {
    val res = new scala.collection.mutable.ArrayBuffer[Vote]
    //val groupInverse : Map[String,GroupInformation] = Map.empty++groupInfo.map{g => g.groupId->g}
    for (satlNo<-satls.indices) {
      val s=satls(satlNo) 
      if (s.numVoters>0) {
        if (usesGroupVotingTickets) {
          val gi : GroupInformation = groupFromID(s.group)
          val numTickets = gi.tickets.length
          if (numTickets==0) println("WARNING: Group "+gi.groupName+" id "+s.group+" has no tickets but "+s.numVoters+" ATL votes in "+meta.electionName)
          else { // ticket exists
            val portion = s.numVoters/numTickets
            val excess = s.numVoters-portion*numTickets
            val choice = if (excess>0) ticketRoundingChoicesMade.getOrElse(gi.groupId,{println(meta.electionName.toString+" ticket "+gi.groupId+" needs a rounding choice between 0 and "+(numTickets-1)+" to deal with "+excess+" excess");0}) else 0
            //println("Group "+s.group+" ticket choice "+choice+" for excess "+excess)
            for (i<-0 until numTickets) {
              val extra = if (excess==0) 0 
                          else if (numTickets==2) { if (i==choice) 1 else 0 }
                          else if (numTickets==3) { if ((i==choice)==(excess==1)) 1 else 0 }
                          else 0
              res+=new Vote(gi.tickets(i),portion+extra,s)
            }        
          }
        } else res+=new Vote(candidatesInGroup(s.group),s.numVoters,s)
      } 
    }
    for (s<-ratls) res+=new Vote(s.groups.flatMap{c=>candidatesInGroup(c.toString)},s.numVoters,s)
    for (b<-btls) res+=b.toVote
    res.toArray
  }
  def candidateIndex(name:String) : CandidateIndex = candidates.indexWhere { _.name==name}
  def numCandidates : Int = candidates.length
  
  /** Same data, should produce same results, but with the id numbers of the candidates reversed. Used for testing... if it doesn't produce the same results, something is wrong. */
  def reverseCandidateOrder : ElectionData = {
    new ElectionData(meta.reverseCandidateOrder,satls,ratls,btls.map{btl => new BTL(btl.candidates.map{numCandidates-1-_},btl.numVoters)},numInformal)
  }

  class TamperWork(val tamperName:String) {
    var excludeVotes : Set[VoteSource] = Set.empty
    var partialExcludeVotes : Map[VoteSource,Int] = Map.empty
    val addVotes = new ArrayBuffer[VoteSource]

    def process[T <: VoteSourceSubsettable[T] : ClassTag](invotes:Array[T]) : Array[T] = {
      val doneFullExclude : Array[T] = invotes.filter{v => !excludeVotes.contains(v)}
      val donePartialExclude : Array[T] =  for (v <-doneFullExclude) yield partialExcludeVotes.get(v) match { case Some(n)=> v.subset(v.n-n); case None => v}
      val addT : Array[T] = addVotes.toArray.collect{case v:T if v.n>0 => v}
      donePartialExclude++addT
    }

    def get : ElectionData = new ElectionData(new ElectionMetadata(meta.electionName.withModification(tamperName),meta.candidates,meta.groupInfo,meta.officialResults,meta.downloadLocation),process(satls),process(ratls),process(btls),numInformal)
  }
  /** Apply a tampering to this dataset */
  def tamper(tampering:Margin,tamperName:String) : ElectionData = {
    val work = new TamperWork(tamperName)
    for (t<-tampering.tamperings;src<-t.src) {
      for (v<-src.allused) {
        work.excludeVotes+=v
        work.addVotes+=v.swap(t.whoFrom,candidates(t.whoFrom),t.whoTo,candidates(t.whoTo))
      }
      for ((n,v)<-src.partiallyUsed) {
        work.partialExcludeVotes+=v->(work.partialExcludeVotes.getOrElse(v,0)+n)
        work.addVotes+=v.swap(t.whoFrom,candidates(t.whoFrom),t.whoTo,candidates(t.whoTo)).subset(n)
      }
    } 
    work.get
  }

  def tamperMichelleFormat(f:File): ElectionData = {
    val work = new TamperWork(f.getName)
    val source = Source.fromFile(f)
    val LineFormat = """\(([\d,]+)\)\s*\:\s*(\d+)""".r
    // .map{v=> ->v.src}
    val existingVotes = new mutable.HashMap[mutable.WrappedArray[Int],ArrayBuffer[VoteSource]]()
    for (v<-makeVotes(Map.empty)) {
      existingVotes.getOrElseUpdate(mutable.WrappedArray.make(v.preferences),new ArrayBuffer[VoteSource]())+=v.src
    }
    try {
      var mode : String = "";
      for (line<-source.getLines) line match {
        case "Add Ballots" => mode=line
        case "Remove Ballots" => mode=line
        case LineFormat(candidateList,ns) =>
          val candidates = candidateList.split(',').map{_.toInt}
          val n = ns.toInt
          if (mode=="Add Ballots") {
            work.addVotes+=new BTL(candidates,n)
          } else if (mode=="Remove Ballots") {
            val key : mutable.WrappedArray[Int] = mutable.WrappedArray.make(candidates)
            val l : ArrayBuffer[VoteSource] = existingVotes.getOrElse(key,throw new Exception("No such vote "+line))
            var togo = n
            for (s<-l) if (togo>0 && !work.excludeVotes(s)) {
              val left = s.n - work.partialExcludeVotes.getOrElse(s,0)
              val used = togo min left
              togo-=used
              val newLeft = left-used
              if (newLeft==0) { work.excludeVotes+=s; work.partialExcludeVotes-=s; }
              else work.partialExcludeVotes+=s-> (s.n-newLeft)
            }
          } else println("Don't know what to do with line "+line)
        case _ => println("Do not understand tamper line "+line)
      }
    } finally { source.close() }
    work.get
  }
  val getSimpleStatistics = new ElectionDataSimpleStatistics(numSATLs,numRATLs,numBTLs,ratls.length,btls.length,totalFormalVotes,numInformal,candidates.length,usesGroupVotingTickets,meta.downloadLocation)
}

class ElectionDataSimpleStatistics(val numSATLs:Int,val numRATLs:Int,val numBTLs:Int,val uniqueRATLs:Int,val uniqueBTLs:Int,val formalVotes:Int,val informalVotes:Int,val numCandidates:Int,val usesGroupVotingTickets:Boolean,val downloadLocation:Array[String])

class Vote(/** ordered list of candidates */ val preferences:Array[CandidateIndex],val numVoters:Int,val src:VoteSource)


trait VoteSource {
  def isATL : Boolean
  /** The number of votes here */
  def n:Int
  def swap(fromWho:Int,candidateFrom:Candidate,toWho:Int,candidateTo:Candidate) : VoteSourceSubsettable[VoteSource]
}

trait VoteSourceSubsettable[+T] extends VoteSource {
  def subset(n:Int) : T
}

/** Single above the line vote for a single party - choose one ticket, basically. */
sealed class SATL(val group:String,val numVoters:Int) extends Dumpable with VoteSourceSubsettable[SATL] {
  override def line: String = ""+group+"\t"+numVoters
  override def isATL: Boolean = true
  override def n: Int = numVoters
  override def subset(n:Int) : SATL = new SATL(group,n)
  override def swap(fromWho:Int,candidateFrom:Candidate,toWho:Int,candidateTo:Candidate): SATL = new SATL(candidateTo.group,numVoters) // not perfect, but as good as likely to get.
}

/** Above the line vote for multiple parties */
sealed class ATL(/** groups listed in preference order */ val groups:Array[GroupID],val numVoters:Int) extends Dumpable with VoteSourceSubsettable[ATL] {
  override def line: String = groups.mkString(" ")+"\t"+numVoters
  override def isATL: Boolean = true
  override def n: Int = numVoters
  override def subset(n:Int) : ATL = new ATL(groups,n)
  override def swap(fromWho:Int,candidateFrom:Candidate,toWho:Int,candidateTo:Candidate) : ATL = { // assume destination is top of chart.
    new ATL(groups.map{c => if (c==candidateFrom.group) candidateTo.group else if (c==candidateTo.group) candidateFrom.group else c},numVoters)
  }
} 

/** Below the line vote */
sealed class BTL(/** candidate ids listed in preference order */ val candidates:Array[CandidateIndex],val numVoters:Int) extends Dumpable with VoteSourceSubsettable[BTL] {
  override def line: String = candidates.mkString(",")+"\t"+numVoters
  override def isATL: Boolean = false
  override def n: Int = numVoters
  override def subset(n:Int) : BTL = new BTL(candidates,n)
  override def swap(fromWho:Int,candidateFrom:Candidate,toWho:Int,candidateTo:Candidate): BTL = new BTL(candidates.map{c => if (c==fromWho) toWho else if (c==toWho) fromWho else c},numVoters)
  def toVote : Vote = new Vote(candidates,numVoters,this)
}

object BTL {
  def atoiOrNegOneIfBlank(s:String) : Int = if (s=="") -1 else s.toInt
  def ofPreferencesInCandidateOrder(prefs:Array[String]) : BTL = ofPreferencesInCandidateOrder(prefs.map{atoiOrNegOneIfBlank}) // see also BelowTheLineBuilderByCandidateID.ofPreferencesInCandidateOrderNotAssumingFormality
  def ofPreferencesInCandidateOrder(prefs:Array[Int]) : BTL = {
    val res = Array.fill(prefs.length)(-1)
    for (i<-prefs.indices) if (prefs(i)>0) res(prefs(i)-1)=i
    new BTL(res.takeWhile{_ >= 0},1)
  }
}

class ActualListOfTamperableVotes(val allused:Array[VoteSource],val partiallyUsed:List[(Int,VoteSource)]) {
  /** Split into a list of length n and a list of all others */
  def split(n:Int) : (ActualListOfTamperableVotes,ActualListOfTamperableVotes) = {
    var togo = n
    val buffer1 = new ArrayBuffer[VoteSource]
    val buffer2 = new ArrayBuffer[VoteSource]
    val partial1 = new ListBuffer[(Int,VoteSource)]
    val partial2 = new ListBuffer[(Int,VoteSource)]
    for (v<-allused) {
      if (togo==0) buffer2+=v
      else if (togo>=v.n) { buffer1+=v; togo-=v.n }
      else {
        partial1+= ((togo,v))
        partial2+= ((v.n-togo,v))
        togo=0
      }
    }
    for (v<-partiallyUsed) {
      if (togo==0) partial2+=v
      else if (togo>=v._1) { partial1+=v; togo-=v._1 }
      else {
        partial1+= ((togo,v._2))
        partial2+= ((v._1-togo,v._2))
        togo=0
      }   
    }
    (new ActualListOfTamperableVotes(buffer1.toArray,partial1.toList),new ActualListOfTamperableVotes(buffer2.toArray,partial2.toList))
  }
}




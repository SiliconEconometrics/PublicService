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
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer
import org.greatcactus.vote.count.federal.Margin

sealed class ElectionMetadata(
                            val name : String,
                            val year : String,
                            val candidates : Array[Candidate],
                            val groupInfo : Array[GroupInformation],
                             /** If present, a list of candidate ids for the official winners, in order of election */
                            val officialResults : Option[Array[Int]]
                          ) {
  val groupFromID : Map[String,GroupInformation] = Map.empty++groupInfo.map{g=>g.groupId->g}
  val groupIndexFromID : Map[String,Int] = Map.empty++(for (i<-0 until groupInfo.length) yield groupInfo(i).groupId->i)
  lazy val groupNameFromID : Map[String,String] = Map.empty++groupInfo.map{g=>g.groupId->g.groupName}+(""->"Ungrouped")
  lazy val candidateIndexFromName : Map[String,Int] = Map.empty++candidates.map{_.name}.zipWithIndex

  val numCandidates = candidates.length
  def reverseCandidateOrder : ElectionMetadata = new ElectionMetadata(name,year,candidates.reverse,groupInfo,officialResults.map{_.map{numCandidates-1-_}})

  def prettyGroupName(groupID:String) : String = groupNameFromID.get(groupID).getOrElse(groupID)
}



sealed class ElectionData(
    val meta : ElectionMetadata,
    val satls : Array[SATL],
    val ratls : Array[ATL],
    val btls : Array[BTL]
    ) {
  def name : String = meta.name
  def year : String = meta.year
  def candidates : Array[Candidate] = meta.candidates
  def groupInfo : Array[GroupInformation] = meta.groupInfo

  def groupFromID = meta.groupFromID
  def groupIndexFromID = meta.groupIndexFromID
  def groupNameFromID = meta.groupNameFromID
  def candidateIndexFromName = meta.candidateIndexFromName

  def numSATLs = satls.map{_.numVoters}.sum
  def numRATLs = ratls.map{_.numVoters}.sum
  def printStatus() {
    println(name)
    println("SATLs : "+numSATLs)
    println("RATLS : "+numRATLs+" num distinct = "+ratls.length)
    println("BTLs : "+btls.length)
    println("Total formal votes : "+totalFormalVotes)
    println("Candidates : " + candidates.size)
    if (consolidatedNumberOfTicketChoiceCombinations>1)  println("Number of ticket start combinations : "+consolidatedNumberOfTicketChoiceCombinations)
  }
  def totalFormalVotes = numSATLs+numRATLs+btls.length
  val usesGroupVotingTickets = groupInfo.exists { _.tickets.length>0 }
  val ticketRoundingChoices : Array[Int] = for (satl<-satls) yield { // number of random choices EC has to make for group voting tickets, by SATL.
    val fromtickets = groupFromID(satl.group).tickets.length max 1 // intrinsic number of choices, but may not crop up
    if (satl.numVoters%fromtickets==0) 1 else fromtickets
  }
  val consolidatedNumberOfTicketChoiceCombinations = ticketRoundingChoices.product
  def makeVotes(ticketRoundingChoices:Map[String,Int]=Map.empty) : Array[Vote] = {
    val res = new scala.collection.mutable.ArrayBuffer[Vote]
    val groupInverse : Map[String,GroupInformation] = Map.empty++groupInfo.map{g => g.groupId->g}
    val groups : Map[String,Array[Int]] = Map.empty++ (for ((group,l)<-candidates.zipWithIndex.groupBy{ _._1.group }) yield (group,l.map{_._2}.toList.sorted.toArray))
    for (satlNo<-0 until satls.length) {
      val s=satls(satlNo) 
      if (s.numVoters>0) {
        if (usesGroupVotingTickets) {
          val gi : GroupInformation = groupInverse(s.group)
          val numTickets = gi.tickets.length
          if (numTickets==0) println("WARNING: Group "+gi.groupName+" id "+s.group+" has no tickets but "+s.numVoters+" ATL votes in "+name)
          else { // ticket exists
            val portion = s.numVoters/numTickets
            val excess = s.numVoters-portion*numTickets
            val choice = if (excess>0) ticketRoundingChoices.getOrElse(gi.groupId,{println(name+" ticket "+gi.groupId+" needs a rounding choice between 0 and "+(numTickets-1)+" to deal with "+excess+" excess");0}) else 0
            //println("Group "+s.group+" ticket choice "+choice+" for excess "+excess)
            for (i<-0 until numTickets) {
              val extra = if (excess==0) 0 
                          else if (numTickets==2) { if (i==choice) 1 else 0 }
                          else if (numTickets==3) { if ((i==choice)==(excess==1)) 1 else 0 }
                          else 0
              res+=new Vote(gi.tickets(i),portion+extra,s)
            }        
          }
        } else res+=new Vote(groups(s.group),s.numVoters,s)
      } 
    }
    for (s<-ratls) res+=new Vote(s.groups.flatMap{c=>groups(c.toString)},s.numVoters,s)
    for (b<-btls) res+=new Vote(b.candidates,1,b)
    res.toArray
  }
  def candidateIndex(name:String) : Int = candidates.indexWhere { _.name==name}
  def numCandidates = candidates.length
  
  /** Same data, should produce same results, but with the id numbers of the candidates reversed. Used for testing... if it doesn't produce the same results, something is wrong. */
  def reverseCandidateOrder : ElectionData = {
    new ElectionData(meta.reverseCandidateOrder,satls,ratls,btls.map{btl => new BTL(btl.candidates.map{numCandidates-1-_})})
  }
  
  /** Apply a tampering to this dataset */
  def tamper(tampering:Margin,tamperName:String) : ElectionData = {
    var excludeVotes : Set[VoteSource] = Set.empty
    var partialExcludeVotes : Map[VoteSource,Int] = Map.empty
    val addVotes = new ArrayBuffer[VoteSource]
    
    def process[T <: VoteSourceSubsettable[T] : ClassTag](invotes:Array[T]) : Array[T] = {
      val doneFullExclude : Array[T] = invotes.filter{v => !excludeVotes.contains(v)}
      val donePartialExclude : Array[T] =  for (v <-doneFullExclude) yield partialExcludeVotes.get(v) match { case Some(n)=> v.subset(v.n-n); case None => v}
      val addT : Array[T] = addVotes.toArray.collect{case v:T if v.n>0 => v}
      donePartialExclude++addT
    }
    
    for (t<-tampering.tamperings;src<-t.src) {
      for (v<-src.allused) {
        excludeVotes+=v
        addVotes+=v.swap(t.whoFrom,candidates(t.whoFrom),t.whoTo,candidates(t.whoTo))
      }
      for ((n,v)<-src.partiallyUsed) {
        partialExcludeVotes+=v->(partialExcludeVotes.getOrElse(v,0)+n)
        addVotes+=v.swap(t.whoFrom,candidates(t.whoFrom),t.whoTo,candidates(t.whoTo)).subset(n)
      }
    } 
    new ElectionData(new ElectionMetadata(name+"_"+tamperName,year,candidates,groupInfo,meta.officialResults),process(satls),process(ratls),process(btls))
  }
}

object ElectionDataFastIO {
  def savePickled(data:ElectionData,file:java.io.File) {
    file.getParentFile.mkdirs()
    savePickled(data,new FileWriter(file))
  }
  def savePickled(data:ElectionData,writer: java.io.Writer) {
    val meta = data.meta
    val w = new PrintWriter(writer)
    def go[T <: Dumpable](a:Array[T],heading:String) {
      w.println(heading)
      w.println(a.length)
      for (e<-a) w.println(e.line)
    }
    w.println(meta.name+"\t"+meta.year)
    go(meta.candidates,"Candidates")
    go(meta.groupInfo,"Groups")
    w.println("Official Results")
    w.println(meta.officialResults.size)
    for (e<-meta.officialResults) w.println(e.mkString(","))
    go(data.satls,"SATLs")
    go(data.ratls,"RATLs")
    go(data.btls,"BTLs")
    w.close()
  }
  def parseCommaSeperatedIntegers(s:String) : Array[Int] = s.split(',').map{_.toInt}
  def loadPickled(file:java.io.File) : ElectionData = (new PickledLoader(file)).loadFull()
  def loadPickledMetadata(file:java.io.File) : ElectionMetadata = (new PickledLoader(file)).loadMeta()
  private class PickledLoader(file:java.io.File) {
    val r = new BufferedReader(new FileReader(file))
    val nameline = r.readLine().split('\t')
    val name = nameline.headOption.getOrElse("")
    val year = if (nameline.length>1) nameline(1) else ""

    import scala.reflect._
    def read[T](f:String=>T)(implicit tag : ClassTag[T]) : Array[T] = {
      val heading = r.readLine()
      val len = r.readLine().toInt
      val res = new Array[T](len)
      for (i<-0 until len) res(i)=f(r.readLine())
      res
    }
    val candidates = read[Candidate]{s=>val ss = s.split('\t'); new Candidate(ss(0),ss(1),ss(2).toInt)}
    val groups = read[GroupInformation]{s=>val ss=s.split('\t'); new GroupInformation(ss(0),if (ss.length==1) "" else ss(1),if (ss.length<3 || ss(2).isEmpty) None else Some(ss(2)),ss.drop(3).map(parseCommaSeperatedIntegers))}
    val officialResults = read[Array[Int]]{parseCommaSeperatedIntegers}
    val meta = new ElectionMetadata(name,year,candidates,groups,officialResults.headOption)

    def loadMeta() : ElectionMetadata = {r.close(); meta }
    def loadFull() : ElectionData = {
      val satls = read[SATL]{s=>val ss=s.split('\t'); new SATL(ss(0),ss(1).toInt)}
      val ratls = read[ATL]{s=>val ss=s.split('\t'); new ATL(ss(0).split(' '),ss(1).toInt)}
      val btls = read[BTL]{s=>new BTL(parseCommaSeperatedIntegers(s))}
      val res = new ElectionData(meta,satls,ratls,btls)
      r.close()
      res
    }
  }
}

trait Dumpable {
  def line:String
}

sealed class Candidate(val name:String,val group:String,val position:Int) extends Dumpable {
  def line = name+"\t"+group+"\t"+position
}
    
class GroupInformation(
    val groupId:String,
    val groupName:String,
    val shortName:Option[String],
    val tickets : Array[Array[Int]] // outer array is ticket number, inner array is preferences list (actual value is a 0 based candidate number)
    ) extends Dumpable {
  def line = groupId+"\t"+groupName+"\t"+shortName.getOrElse("")+"\t"+tickets.map{_.mkString(",")}.mkString("\t")
}



class Vote(/** ordered list of candidates */ val preferences:Array[Int],val numVoters:Int,val src:VoteSource)


trait VoteSource {
  def isATL : Boolean
  /** True if it is possible for the vote to be tampered with without easy detection, assuming it is not the first preference */
  def isTamperable(/** True if the position being considered for modification is plausibly in the first group of a RATL vote */ couldBeInFirstGroup:Boolean,/** True if only want ATL, false if only want BTL */wantATL:Boolean) : Boolean
  /** The number of votes here */
  def n:Int
  def swap(fromWho:Int,candidateFrom:Candidate,toWho:Int,candidateTo:Candidate) : VoteSourceSubsettable[VoteSource]
}

trait VoteSourceSubsettable[+T] extends VoteSource {
  def subset(n:Int) : T
}
sealed class SATL(val group:String,val numVoters:Int) extends Dumpable with VoteSourceSubsettable[SATL] {
  def line = ""+group+"\t"+numVoters
  override def isATL = true
  override def isTamperable(couldBeInFirstGroup:Boolean,wantATL:Boolean) = false
  override def n = numVoters
  override def subset(n:Int) = new SATL(group,n)
  override def swap(fromWho:Int,candidateFrom:Candidate,toWho:Int,candidateTo:Candidate) = new SATL(candidateTo.group,numVoters) // not perfect, but as good as likely to get.
}

sealed class ATL(/** groups listed in preference order */ val groups:Array[String],val numVoters:Int) extends Dumpable with VoteSourceSubsettable[ATL] {
  def line = groups.mkString(" ")+"\t"+numVoters
  override def isATL = true
  override def isTamperable(couldBeInFirstGroup:Boolean,wantATL:Boolean) = wantATL && !couldBeInFirstGroup
  override def n = numVoters
  override def subset(n:Int) = new ATL(groups,n)
  override def swap(fromWho:Int,candidateFrom:Candidate,toWho:Int,candidateTo:Candidate) = { // assume destination is top of chart. 
    new ATL(groups.map{c => if (c==candidateFrom.group) candidateTo.group else if (c==candidateTo.group) candidateFrom.group else c},numVoters)
  }
} 

sealed class BTL(/** candidate ids listed in preference order */ val candidates:Array[Int]) extends Dumpable with VoteSourceSubsettable[BTL] {
  def line = candidates.mkString(",")
  override def isATL = false
  override def isTamperable(couldBeInFirstGroup:Boolean,wantATL:Boolean) = !wantATL
  override def n = 1
  override def subset(n:Int) = new BTL(candidates)
  override def swap(fromWho:Int,candidateFrom:Candidate,toWho:Int,candidateTo:Candidate) = new BTL(candidates.map{c => if (c==fromWho) toWho else if (c==toWho) fromWho else c})
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



class VoteInterpreter(groups:Array[GroupInformation],val numCandidates:Int) {
    val btl = new scala.collection.mutable.HashMap[Long,BelowTheLineBuilder]
    val atl = new scala.collection.mutable.HashMap[Int,AboveTheLineBuilder]
    def numGroups = groups.length
    val satlCounts = new Array[Int](numGroups)
    val extraBTLs = new ArrayBuffer[BTL]
    val extraATLs = new ArrayBuffer[String]
    val groupLookup = Map(groups.map{_.groupId}.zipWithIndex : _*)
    var officialResults : Option[Array[Int]] = None
  
    def addBTL(btl:BTL) { extraBTLs+=btl }
    def addBTL(ballotID:Long,candidate:Candidate,preferenceNumber:Int) {
              val v = btl.getOrElseUpdate(ballotID,new BelowTheLineBuilder(ballotID,numCandidates))
              v.addVote(candidate,preferenceNumber)      
    }
    def addRATL(spaceSepVotes:String) { extraATLs+=spaceSepVotes }
    def addRATL(ballotID:Int,groupCode:String,preferenceNumber:Int) {
              val v = atl.getOrElseUpdate(ballotID,new AboveTheLineBuilder(numGroups))
              v.addVote(groupCode,preferenceNumber)      
    }
    def addSATL(groupIndex:Int,n:Int=1) {
      satlCounts(groupIndex)+=n
    }
    def addSATL(group:String) {
      addSATL(groupLookup(group),1)
    }
    def getData(orderedCandidates:Array[Candidate],name:String,year:String) : ElectionData = {
      //println("SATLs : "+satlCounts.sum)
      //println("RATLs : "+atl.size)
      //println("BTLs : "+btl.size)
      //println("candidates : "+orderedCandidates.length)

      val candidateToIndex : Map[Candidate,Int]=Map.empty++orderedCandidates.zipWithIndex
      val satls = for ((count,index)<-satlCounts.zipWithIndex) yield new SATL(groups(index).groupId,count)
      val canonatls = {
        val strings = (for (v<-atl.values) yield v.get).toList++extraATLs
        val bunched = for ((s,l)<-strings.groupBy { a => a }) yield new ATL(s.split(' '),l.length)
        bunched.toArray
      }
      val btls = (for (v<-btl.values) yield v.get(candidateToIndex)).toArray++extraBTLs
      new ElectionData(new ElectionMetadata(name,year,orderedCandidates,groups,officialResults),satls.toArray,canonatls,btls)
    }
}


class AboveTheLineVote(val ballotID:Int,/** groups voted for, in order */val groups:List[Int])

/** Helper to build vote lists, taking duplicates into account (votes below and including duplicate numbers are ignored */
class GeneralVoteBuilder[T <: AnyRef : ClassTag ](numPossibilities:Int) {
  val preferences : Array[T] = new Array[T](numPossibilities) // preferences(0) is who is given the first preference, 1 second, etc.
  var duplicate = Integer.MAX_VALUE // the index of the smallest duplicated preference

  /** record a (1 based) vote preferenceNumberGiven cast for whofor */
  def addVote(whofor:T,preferenceNumberGiven:Int) {
    if (preferenceNumberGiven>0 && preferenceNumberGiven<=numPossibilities) {
      if (preferences(preferenceNumberGiven-1)!=null) {
        //println("Found duplicate preference "+preferenceNumberGiven+" for "+whofor+" and "+preferences(preferenceNumberGiven-1))
        duplicate = duplicate min (preferenceNumberGiven-1)
      }
      preferences(preferenceNumberGiven-1)=whofor
    } else println("Found bogus preference "+preferenceNumberGiven+" for "+whofor) 
  }
  
  /** Get the first consecutive set of preferences not containing duplicates */
  def unduplicated : Array[T] = {
    val nonnull = preferences.takeWhile(_ !=null)
    if (nonnull.length>duplicate) nonnull.slice(0,duplicate) else nonnull 
  }
}

class AboveTheLineBuilder(numGroups:Int) extends GeneralVoteBuilder[String](numGroups) {
  def get : String = unduplicated.mkString(" ")
}

class BelowTheLineBuilder(val ballotID:Long,numCandidates:Int) extends GeneralVoteBuilder[Candidate](numCandidates) {
  def get(candidateIndex: Candidate=>Int) : BTL = new BTL(unduplicated.map(candidateIndex))
}

class BelowTheLineBuilderByCandidateID(numCandidates:Int) extends GeneralVoteBuilder[Integer](numCandidates) {
  def get : BTL = new BTL(unduplicated.map{_.toInt})
}



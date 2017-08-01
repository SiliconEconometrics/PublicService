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
package org.greatcactus.vote.count.wa

import java.io.File
import scala.io.Source
import java.util.zip.ZipInputStream
import java.io.FileInputStream
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import scala.util.Random
import java.io.InputStreamReader
import java.io.InputStream
import java.io.BufferedReader
import scala.collection.mutable.ArrayBuffer
import scala.xml.XML
import org.greatcactus.vote.count._
import org.greatcactus.vote.count.federal.CSVHelper
import org.greatcactus.vote.count.MainDataTypes.CandidateIndex

object WA2013ElectionData {

  val sourceDir = new File("""Elections/WA/2013/""")
  
  
  def loadRegionRaw(region:String) = {
    val officialResult = loadDetailedCalculationStepsRaw(region)
    val (groups,ticketIds) = loadGroups(region,officialResult.candidates.map{_.name})
    val interpreter = new VoteInterpreter(groups,officialResult.candidates.length)
    loadVotes(region,interpreter,ticketIds)
    val data = interpreter.getData(officialResult.candidates, region,"2013")
    (data,officialResult)
  }
  
  def loadGroups(region:String,candidateNames:Array[String]) : (Array[GroupInformation],Map[String,Int]) = {
    val helper = CSVHelper(new File(sourceDir,region+"_TicketsReport.csv"),11)
    var groupName : String = ""
    var groupId : String = ""
    var groupShort : String = ""
    val ticket : Array[Int] = new Array[Int](candidateNames.length)
    val nameToIndex : Map[String,Int] = Map(candidateNames.zipWithIndex :_*)
    val list = new ArrayBuffer[GroupInformation]
    //val candidates = new ArrayBuffer[GroupInformation]
    def finishGroup() {
      list += new GroupInformation(groupId,groupName,if (groupShort.isEmpty) None else Some(groupShort),Array(ticket.toList.toArray))
      groupName=""
      groupId=""
      groupShort=""
    }
    var ticketIds : Map[String,Int] = Map.empty
    for (l<-helper) l(0) match {
      case "Group Name" => finishGroup(); groupName=l(2)
      case "Group Abbreviated Name" => groupShort=l(2)
      case "Group Ballot Paper Order" => groupId=l(2)
      case "" =>
      case "Candidates:" =>
      case pref => 
        val pi = pref.toInt-1
        if (pi==0) { // Their naming conventions are inconsistent. I'm going out on a limb and assuming the first preference on the ticket is from the party in question. Also spaces are stuffed around.
          val groupIdForTicketList = (if (l(3).isEmpty) l(1) else l(3)).replaceAll(" ","")
          ticketIds+= groupIdForTicketList->list.length
          println(groupIdForTicketList)
        }
        ticket(pi) = nameToIndex(l(1))
        //if (l()==groupName) candidates+=new Candidate(candidateNames(candidates.length),groupId,)
    }
    finishGroup()
    (list.toArray,ticketIds)
  }
  
  def loadVotes(region:String,interpreter:VoteInterpreter,ticketIds:Map[String,Int]) {
    val helper = CSVHelper(new File(sourceDir,region+"_BatchesReport.csv"),3)
    def btl(l:Array[String]) {
       if (l(2).isEmpty) try { // formal
         val builder = new BelowTheLineBuilderByCandidateID(interpreter.numCandidates)
        //  val candidates = new Array[CandidateIndex](interpreter.numCandidates)
         for (i<-0 until interpreter.numCandidates) {
           val v = l(4+i)
           if (!v.isEmpty) builder.addVote(i,v.toInt)
         }
         interpreter.addBTL(builder.get)
       } catch { case e:Exception => println("Error reading "+region+"_BatchesReport.csv BTL vote line number "+helper.lineCount); }
    }
    for (l<-helper) l(0) match {
      case "" => btl(l)
      case "Y" => btl(l)
      case "Batch #:" =>
      case "District Name:" =>
      case "Ballot Paper" =>
      case "Changed" =>
      case "Totals" =>
      case party => interpreter.addSATL(ticketIds(party.replaceAll(" ","")), l(1).toInt)
    }
  }
  
  def loadDetailedCalculationStepsRaw(region:String) : WA2013WholeCount = {
    val helper = CSVHelper(new File(sourceDir,region+"_DetailedCalculationSteps.csv"),13,',',"ISO8859_1")
    val candidateNames : Array[String] = helper.splitHeading(5).drop(7).dropRight(3).map{_.replace('\u00a0',' ')}
    val candidateGroups : Array[String] = helper.splitHeading(4).drop(7).dropRight(3)
    val candidates : Array[Candidate] = { // not perfect, if a group name is "blank", they will be dumped in the prior group. But this doesn't really matter for other than GUI, and the 2013 data is used for testing the algorithm rather than actual use. But for niceness sake, I modified the data file with a placeholder.
      var groupId = 0
      var groupPosition = 0
      for (i<-0 until candidateNames.length) yield {
        if (candidateGroups(i).isEmpty) groupPosition+=1 else { groupId+=1; groupPosition=0 }
        new Candidate(candidateNames(i),groupId.toString,groupPosition)
      }
    }.toArray
    // for (s<-candidateNames(0)) println("char is "+s.toInt)
    val quotaMatchingString = """Quota = (\d+)/\((\d+)\+1\)\+1 = (\d+)""".r
    val quotaMatchingString(numFormalVotes,numPositions,quota) = helper.splitHeading(11).head
        println("numFormalVotes = "+numFormalVotes)
        println("numPositions = "+numPositions)
        println("quota = "+quota)
    var who : String = ""
    var what : String = ""
    var prevCount : String = null; // null means no previous count, or total
    var candidatePapers : Array[Int] = null
    var candidatePapersTransferred : Array[Int] = null
    var candidateVotes : Array[Int] = null
    var candidateVotesTransferred : Array[Int] = null
    var transferValue : Double = 0.0
    var lostVotes : Int = 0
    var lostVotesCum : Int = 0
    val countList = new ArrayBuffer[WA2013SingleCount]
    def saveCount() {
      countList+=new WA2013SingleCount(prevCount,who,what,candidatePapersTransferred,candidatePapers,candidateVotesTransferred,candidateVotes,lostVotes,lostVotesCum)
      candidatePapers = null
      candidateVotes = null
      candidateVotesTransferred = null
      candidatePapersTransferred = null
    }
    def parsePossiblyBlank = { s:String =>  if (s.isEmpty) 0 else s.toInt }
    for (l<-helper) {
        if (!l(0).isEmpty()) {
          if (l(1).isEmpty()) what="First Preferences"
          else { what = l(1); who=l(2); }
        }
        if (!l(2).isEmpty()) { 
          if (prevCount!=null) saveCount()
          if (l(2)=="Total") prevCount=null
          else { 
            prevCount = l(2)
            transferValue = l(6).toDouble
          }
        }
        if (prevCount!=null) {
          val numMeaning = l(4)
          if (!numMeaning.isEmpty) {
             val nums = l.drop(7).dropRight(3).map{parsePossiblyBlank}
             val lostFractions = parsePossiblyBlank(l(l.length-3))
             numMeaning match {
               case "Votes" => candidatePapers = nums; candidateVotes = nums; candidateVotesTransferred=nums; candidatePapersTransferred=nums
               case "BPs Rec'd" => candidatePapersTransferred=nums
               case "Progress BPs" => candidatePapers=nums
               case "Votes Rec'd" => candidateVotesTransferred=nums; lostVotes=lostFractions
               case "Progress Votes" => candidateVotes=nums; lostVotesCum=lostFractions
               case "Lost Fractions" => for (i<-0 until nums.length) { candidateVotes(i)+=nums(i); candidateVotesTransferred(i)+=nums(i) } ; lostVotes+=lostFractions; lostVotesCum+=lostFractions
               case _ => throw new IllegalArgumentException("Unanticipated number meaning "+numMeaning)
             }
            
          }
        }
    }
    new WA2013WholeCount(candidates,numFormalVotes.toInt,numPositions.toInt,quota.toInt,countList.toArray)
  }
  
 
  
  

}

class WA2013WholeCount(val candidates : Array[Candidate],val numFormalVotes:Int,val vacancies:Int,val quota:Int,val counts:Array[WA2013SingleCount])
class WA2013SingleCount(val countName:String,val who:String,val what:String,val papersDelta:Array[Int],val papersCum:Array[Int],val votesDelta:Array[Int],val votesCum:Array[Int],val lostDelta:Int,val lostCum:Int)

object WA2013 extends App {
  val reportDir = new File("WA2013Reports")

  val (data,officialResult) = WA2013ElectionData.loadRegionRaw("Smet")
  data.printStatus()
      val ticketRoundingChoices:Map[String,Int] = Map.empty
    val ecDeemedOrder:Seq[Int] = List()
    val worker = new WAElectionHelper (data,officialResult.vacancies,ticketRoundingChoices,ecDeemedOrder,false,Set.empty)
    worker.run()
    ElectionReport.saveReports(new File(reportDir,data.name),worker.report,data)

}

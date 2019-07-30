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
package org.greatcactus.vote.count.wa.parsing

import java.io.File

import org.greatcactus.vote.count.MainDataTypes.CandidateIndex
import org.greatcactus.vote.count._
import org.greatcactus.vote.count.ballots._
import org.greatcactus.vote.count.ballots.parsing.{CSVHelper, VoteInterpreter}

import scala.collection.mutable.ArrayBuffer
import scala.xml.XML

object WA2017ElectionData {

  val sourceDir = new File(IOUtil.baseDir,"""Elections/WA/2017""")
  val namePrefix = "11 March 2017 State General Election - "
  val extractCategoryVotes = false // used for diagnostics, not needed for results.

  def loadAllRaw() : List[WARegion] = {
    def getXML(name:String) = XML.loadFile(new File(sourceDir,namePrefix+name+".xml"))
    val candidatesXML = getXML("CANDIDATE SETUP")
    val regions : Map[String, WARegionWork] = Map((
      for (region<- candidatesXML \\ "ElectionRegion") yield {
        val regionname = (region \ "@Name").text
        //println("Got region "+regionname)
        val enrolment = (region \ "@Enrolment").text.toInt
        val lc = region \ "LC"
        val vacancies = (lc \ "@Vacancies").text.toInt
        val groupsWithoutTickets = for (g<- lc \ "Group") yield new GroupInformation((g \ "@BallotPaperOrder").text,(g \ "@BallotPaperName").text,Some((g \ "@WAECAbbreviation").text),Array())
        val candidates = for (c<- lc \ "Candidate") yield new Candidate((c \ "@BallotPaperName").text,(c \ "@GroupBallotPaperOrder").text,(c \ "@BallotPaperOrder").text.toInt-1)
        //for (c<-candidates) println(c.name)
        regionname -> new WARegionWork(regionname,vacancies,enrolment,candidates.toArray,groupsWithoutTickets.toArray)
      }
    ).toList :_*)
    if (true) { // add tickets
      val ticketsXML = getXML("GROUP AND TICKET SETUP") 
      for (region<- ticketsXML \\ "ElectionRegion")  {
        val regionname = (region \ "@Name").text
        //println("Got pass2 region "+regionname)
        val work = regions(regionname)
        val candidateIndex = Map((for ((c,ind)<-work.candidates.zipWithIndex) yield c.name->ind).toList :_*)
        for (g<- region \ "LC" \ "Group") {
          val groupIndS = (g \ "@BallotPaperOrder").text
          val ticket = new Array[Int](work.candidates.length)
          for (tp<-g \ "TicketPreference") ticket((tp \ "@Preference").text.toInt-1) =  candidateIndex((tp \ "@CandidateBallotPaperName").text)
          //println("Found ticket for "+groupIndS+" length "+ticket.length+" : "+ticket)
          //println(g)
          if (ticket.length==0) throw new Exception
          work.addTicket(groupIndS, ticket)
        }
      }
    }
    if (true) { // add ATL votes
      val votesXML = getXML("LC VERBOSE RESULTS")
      for (region<- votesXML \\ "ElectionRegion")  {
        val regionname = (region \ "@Name").text
        //println("Got pass3 region "+regionname)
        val work = regions(regionname)
        val district = region \ "LC" \ "DistrictVotes"
        val districtname = (district \ "@DistrictName").text
        if (districtname!=regionname) throw new IllegalArgumentException("District "+districtname+" region "+regionname)
        for (gv<-district \ "GroupVotes") {
          work.addSATL((gv \ "@GroupBallotPaperOrder").text, (gv \ "@TicketVotes").text.toInt)
        }
        if (extractCategoryVotes) {
          for (category <- region \ "LC" \\ "CategoryVotes") {
            val catname = (category \ "@CategoryName").text
            val btlPartyVotes: List[Int] = (for (gv<-category \ "GroupVotes") yield (gv \ "@NonTicketVotes").text.toInt).toList
            // println(catname+"  "+btlPartyVotes.mkString(","))
            work.btlsToCategory.getOrElseUpdate(btlPartyVotes,new FoundCategory(btlPartyVotes)).where+=catname
          }
        }
      }      
    }
    if (true) { // add BTL votes
      for ((_,region)<-regions) {
        val helper = CSVHelper(new File(sourceDir,"CountWA Export - 11 March 2017 State General Election."+region.name+" Region.(Ballot-Papers).csv"),1)
        var lastBatch = -100
        var byParty = new Array[Int](region.groups.length)
        if (extractCategoryVotes) println("Region "+region.name)
        def finishedBatch() {
          val btlPartyVotes: List[Int] =byParty.toList
          val found = region.btlsToCategory.getOrElseUpdate(btlPartyVotes,new FoundCategory(btlPartyVotes))
          found.batches+=lastBatch
          println("Batch "+lastBatch+" "+btlPartyVotes.mkString(",")+"  "+found.where.mkString(" ; "))
        }
        for (line<-helper) if (line(5)=="0") { // check formal column
          // candidates start from column 6. Column indicated candidate.
          val btl = BTL.ofPreferencesInCandidateOrder(line.drop(6).map{s=>if (s.isEmpty) -1 else s.toInt})
          if (extractCategoryVotes) {
            val batch = line(0).toInt
            if (lastBatch<batch-1) {
              if (lastBatch!= -100) finishedBatch()
              for (i<-byParty.indices) byParty(i)=0
            }
            lastBatch=batch
            byParty(region.groupLookup(region.candidates(btl.candidates(0)).group))+=1
          }
          region.addBTL(btl)
        }
        if (extractCategoryVotes) {
          finishedBatch()
          for (found<-region.btlsToCategory.values) {
            if (found.where.length>found.batches.length) println("Categories "+found.where.mkString(",")+" in "+found.batches.length+" batch(es) "+found.batches.mkString(",")+"  signature "+found.btlVotesByParty.mkString(","))
          }
        }
      }
    }
    regions.values.map{_.toRegion}.toList
  }

  class FoundCategory(val btlVotesByParty:List[Int]) {
    val where = new collection.mutable.ListBuffer[String] // Categories
    val batches = new collection.mutable.ListBuffer[Int] // batches
  }
  class WARegionWork(val name:String,val vacancies:Int,val enrolment:Int,val candidates:Array[Candidate],val groups:Array[GroupInformation]) {
    val helper = new VoteInterpreter(groups,candidates.length)
    def groupLookup(groupId:String) : Int = helper.groupLookup(groupId)
    def addSATL(groupId:String,n:Int) { helper.addSATL(groupLookup(groupId), n)}
    def addBTL(btl:BTL) { helper.addBTL(btl)}
    def addTicket(groupId:String,ticket:Array[Int]) {
      val groupInd = groupLookup(groupId)
      val oldg = groups(groupInd)
      groups(groupInd) = new GroupInformation(oldg.groupId,oldg.groupName,oldg.shortName,oldg.tickets:+ticket)
      // println(groups(groupInd).line)
    }
    def toRegion = new WARegion(helper.getData(candidates, new ElectionName("2017","WAEC","WA",name),Array()),vacancies,enrolment)
    val btlsToCategory = new collection.mutable.HashMap[List[Int],FoundCategory]

  }
  

}

class WARegion(val data:ElectionData,val vacancies:Int,val enrolment:Int)

// official results
object WA2017OfficialResults {
  val usedOrders : Map[String,List[CandidateIndex]] = Map("North Metropolitan"->List(37,23),"South West"->List(38,25,27,44,36),"South Metropolitan"->List(34,10),"Agricultural"->List(35,42,20,29,44,46,33,40,5,7,48,31),"Mining and Pastoral"->List(3,47,22,33))

  def toStringBlankIs0(s:String) : Int  = if (s.isEmpty) 0 else s.replace(",","").toInt
  def load(region:WARegion): WA2013WholeCount = {
    val helper = CSVHelper(new File(WA2017ElectionData.sourceDir,region.data.meta.electionName.electorate+"_LCDetailedResults2017.csv"),13)
    val quotaMatchingString = """Quota = (\d+)/\((\d+)\+1\)\+1 = (\d+)""".r
    val quotaMatchingString(numFormalVotes,numPositions,quota) = helper.splitHeading(11).head.replace(",","")
    val numCandidates = region.data.numCandidates
    println("numFormalVotes = "+numFormalVotes)
    println("numPositions = "+numPositions)
    println("quota = "+quota)
    var who : String = ""
    var what : String = ""
    var candidateVotes : Array[Int] = null
    var candidateVotesTransferred : Array[Int] = null
    val electedCandidates = new ArrayBuffer[CandidateIndex]
    var lostVotes : Int = 0
    var lostVotesCum : Int = 0
    val countList = new ArrayBuffer[WA2013SingleCount]
    def saveCount() {
      countList+=new WA2013SingleCount(null,who,what,null,null,candidateVotesTransferred,candidateVotes,lostVotes,lostVotesCum,electedCandidates.toArray)
      candidateVotes = null
      candidateVotesTransferred = null
      electedCandidates.clear()
    }
    def parsePossiblyBlank = { s:String =>  if (s.isEmpty) 0 else s.toInt }
    var lastlineHadNoWhoAndNoElected=false
    var startCandidatesColumn = if (helper.splitHeading(5)(2).isEmpty) 3 else 2
    for (l<-helper) if (!l(0).startsWith("* - Where an excluded")) {
      if (lastlineHadNoWhoAndNoElected && l(0).isEmpty) { l(0)=who; l(1)=what }
      if (!l(0).isEmpty) { // parse the first line
        if (!who.isEmpty) saveCount()
        who = l(0)
        what = l(1)
        candidateVotesTransferred = l.slice(startCandidatesColumn,startCandidatesColumn+numCandidates).map{toStringBlankIs0}
        lostVotes = parsePossiblyBlank(l(startCandidatesColumn+numCandidates))
      } else {
        // see if cumulative sums available
        val cumsums = l.slice(startCandidatesColumn,startCandidatesColumn+numCandidates).map{toStringBlankIs0}
        if (cumsums.max>0) {
          candidateVotes=cumsums
          lostVotesCum=parsePossiblyBlank(l(startCandidatesColumn+numCandidates))
        }
      }
      val electedS = l(2+startCandidatesColumn+numCandidates)
      if (!electedS.isEmpty) electedCandidates+=region.data.candidateIndexFromName(electedS.dropWhile(_ !=' ').trim)
      lastlineHadNoWhoAndNoElected= l(0).isEmpty && electedS.isEmpty
    }
    saveCount()
    new WA2013WholeCount(region.data.candidates,numFormalVotes.toInt,numPositions.toInt,quota.toInt,countList.toArray)
  }


}


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
package org.greatcactus.vote.count.vic.parsing

import java.io.File

import org.greatcactus.vote.count.MainDataTypes.CandidateIndex
import org.greatcactus.vote.count.ballots.GroupInformation.GroupID
import org.greatcactus.vote.count.ballots._
import org.greatcactus.vote.count.ballots.parsing._

import scala.collection.mutable.ArrayBuffer

object TestVicData extends App {
  val region = "Eastern Metropolitan"
  println(Vic2014Data.getCandidateNames(region).mkString(";"))

  Vic2014Data.load(region).printStatus()
}

class VicElectionSpec(override val region:String, val loader:VicElectionDataLoader) extends ElectionSpecification {
  override def jurisdiction: String = "Victoria"
  override def year: String = loader.year
  override def metadataAvailable: Boolean = true
  override def dataAvailable: Boolean = loader.dataAvailable(region)
  override def iterateOverRawBTLDataAvailable: Boolean = true
  override def metadata: ElectionMetadata = if (dataAvailable) loader.loadJustMetadata(region) else loader.loadRawMetadata(region)
  override def data: ElectionData = loader.load(region)
  override def getIterateOverRawBTLData: IterateOverRawBTLData = loader.iterator(region,metadata)
  override def ticketRoundingChoicesMade: Map[GroupID, CandidateIndex] = Map.empty
  override def numToBeElected: CandidateIndex = 5
}

object Vic2014Data extends VicElectionDataLoader("2014") {

}

class VicElectionDataLoader(val year:String) extends CachedElectionDataLoader("VIC/State"+year) with SetOfConcurrentElectionsSpecification {
  val regions: Array[String] = Array("Eastern Metropolitan","Eastern Victoria","Northern Metropolitan","Northern Victoria","South-Eastern Metropolitan","Southern Metropolitan","Western Metropolitan","Western Victoria")
  def dataAvailable(state: String): Boolean = true
  override def jurisdiction:String = "Victoria"
  def availableRegions:Seq[String] = regions
  def getSpec(region:String) : ElectionSpecification = new VicElectionSpec(region,this)

  def paperDetails(region:String): File = rel("Ballot Paper Details - "+region+" Region.csv")
  def distPrefs(region:String): File = rel(region+" Distribution of Preferences Report.csv")

  def getCandidateNames(region:String) : Array[String] = CSVHelper(distPrefs(region),11).readSingleLine().drop(4).dropRight(4).filter(_.nonEmpty)

  def processPrefs(region:String, helper: VoteInterpreter): Unit = {
    val prefsfile = CSVHelper(paperDetails(region),11)
    for (line<-prefsfile) {
      val c0 = line(0)
      if (line.forall(_.isEmpty) || c0.startsWith("Batch No.") || c0=="Ballot Paper Preferences Recorded Against Candidates in Ballot Paper Order" ||c0=="Informal batch - no ballot papers") {} // do nothing with blank line
      else if (c0.forall(_.isDigit)) {
        val vote = BelowTheLineBuilderByCandidateID.ofPreferencesInCandidateOrderNotAssumingFormality(line.tail,helper.numCandidates)
        if (vote.candidates.length>=5) helper.addBTL(vote) else helper.addInformal(1)
        // some particular vote
      } else {
        println("Could not understand line "+line.mkString(","))
      }
    }
  }

  def iterator(region:String,metadata : ElectionMetadata) : IterateOverRawBTLData = new IterateOverRawBTLData {
    override def meta : ElectionMetadata = metadata
    var rowsSoFar : Int = 0
    private var batchNumber : String = _
    private var paperNumber : String = _

    def foreach(f:Array[String]=>Unit) {
      rowsSoFar = 0
      val prefsfile = CSVHelper(paperDetails(region),11)
      for (line<-prefsfile) {
        val c0 = line(0)
        if (c0.startsWith("Batch No.")) batchNumber=c0.substring(9).trim
        if (line.forall(_.isEmpty) ||  c0=="Ballot Paper Preferences Recorded Against Candidates in Ballot Paper Order" ||c0=="Informal batch - no ballot papers") {} // do nothing with blank line
        else if (c0.forall(_.isDigit)) {
          paperNumber = c0
          f(line.tail)
          rowsSoFar+=1
        } else {
          println("Could not understand line "+line.mkString(","))
        }
      }
    }

    override def currentRowMetadata: Map[String, String] = Map("Batch"->batchNumber,"Paper"->paperNumber)
  }
  override def loadRaw(region:String) : ElectionData = {
    val candidates = for (n<-getCandidateNames(region)) yield new Candidate(n,"",0)
    val helper = new VoteInterpreter(Array(),candidates.length)

    processPrefs(region,helper)
    val name = new ElectionName(year,"VEC","Victorian State",region)
    helper.getData(candidates,name,Array())
  }
  def loadRawMetadata(state:String) : ElectionMetadata = throw new IllegalArgumentException("Not implemented")
}


object Vic2014OfficialResults {
  // val usedOrders : Map[String,List[CandidateIndex]] = Map("North Metropolitan"->List(37,23),"South West"->List(38,25,27,44,36),"South Metropolitan"->List(34,10),"Agricultural"->List(35,42,20,29,44,46,33,40,5,7,48,31),"Mining and Pastoral"->List(3,47,22,33))

  def toStringBlankIs0(s:String) : Int  = if (s.isEmpty) 0 else s.replace(",","").toInt
  def load(region:String): VicWholeCount = {
    val helper = CSVHelper(Vic2014Data.distPrefs(region),13)
    def extractHeaderNum(line:Int) : Int = helper.splitHeading(line).head.split(':')(1).trim.toInt
    val numFormalVotes = extractHeaderNum(5)
    val quota = extractHeaderNum(8)
    val membersMatchingString = """Election of (\d+) Members""".r
    val membersMatchingString(numPositions) = helper.splitHeading(4).head.replace(",","")
    val candidateNames = Vic2014Data.getCandidateNames(region)
    val columnHeadings = helper.splitHeading(11).map{_.trim.replace("\n","")}
    def columnFor(s:String) : Int = columnHeadings.indexOf(s)
    val columnForCandidate = (candidateNames++Array("Gain/Loss","Exhausted")).map{columnFor}
    val columnForCountNo = columnFor("Count No.")
    val columnForCountDetails = columnFor("Count Details")
    val columnForTransferValue = columnFor("Transfer Value")
    val columnForRowMeaning = columnForTransferValue+1
    val columnForElected = columnFor("Candidates provisionally elected at this count")
    val numCandidates = candidateNames.length
    println("numFormalVotes = "+numFormalVotes)
    println("numPositions = "+numPositions)
    println("quota = "+quota)
    var candidateVotes : Array[Int] = null
    var papersTransferred : Array[Int] = null
    var candidateVotesTransferred : Array[Int] = null
    val electedCandidates = new ArrayBuffer[CandidateIndex]
    val countList = new ArrayBuffer[VicSingleCount]
    var countName : String = null
    var who : String = null
    var what : String = null
    var transferValue = 0.0
    var fromCounts : Array[String] = null
    def saveCount() {
      countList+=new VicSingleCount(countName,who,what,transferValue,fromCounts,papersTransferred,candidateVotesTransferred,candidateVotes,electedCandidates.toArray)
      candidateVotes = null
      candidateVotesTransferred = null
      transferValue = 0.0
      papersTransferred = null
      who = null
      what = null
      fromCounts = null
      electedCandidates.clear()
    }

    val surplusRegExp = """Surplus of (.+), (\d+) ballot papers distributed""".r
    val exclusionRegExp = """Exclusion of (.+), (\d+) ballot papers from count ([\d,]+)""".r
    for (l<-helper) {
      if (l(columnForCountNo).nonEmpty) countName=l(columnForCountNo)
      l(columnForCountDetails) match {
        case "1st Preferences" => what="first"
        case surplusRegExp(name,_) => who=name; what="surplus"
        case exclusionRegExp(name,_,fromCountsS) => who=name; what="exclusion"; fromCounts=fromCountsS.split(',')
        case "" =>
        case s => println("Unknown count details "+s)
      }
      val countNo = toStringBlankIs0(l(columnForCountNo))
      if (countNo>1) { saveCount(); transferValue=l(columnForTransferValue).toDouble }
      val storeNums : Array[Int] = l(columnForRowMeaning) match {
        case "" => candidateVotes = new Array[Int](numCandidates+2); candidateVotes
        case "PTotal" => candidateVotes = new Array[Int](numCandidates+2); candidateVotes
        case "BPs" =>   papersTransferred = new Array[Int](numCandidates+2); papersTransferred
        case "Value" =>   candidateVotesTransferred = new Array[Int](numCandidates+2); candidateVotesTransferred
      }
      for (i<-columnForCandidate.indices) storeNums(i)=toStringBlankIs0(l(columnForCandidate(i)))
      for (electedNames<-l(columnForElected).split(',').grouped(2)) electedCandidates+=candidateNames.indexOf(electedNames.mkString(",").trim)
    }
    saveCount()
    new VicWholeCount(candidateNames,numFormalVotes.toInt,numPositions.toInt,quota.toInt,countList.toArray)
  }


}

class VicWholeCount(val candidates : Array[String],val numFormalVotes:Int,val vacancies:Int,val quota:Int,val counts:Array[VicSingleCount])
class VicSingleCount(val countName:String,val who:String,val what:String,val transferValue:Double,val fromCounts:Array[String],val papersDelta:Array[Int],val votesDelta:Array[Int],val votesCum:Array[Int],val electedCandidates:Array[CandidateIndex])


object FederalJurisdictionSpecification extends ElectionJurisdictionSpecification {
  override def jurisdiction: String = "Victoria"
  override def availableYears: Seq[String] = List("2014")

  override def getSpec(year: String): SetOfConcurrentElectionsSpecification = year match {
    case "2014" => Vic2014Data
  }
}


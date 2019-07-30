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
package org.greatcactus.vote.count.federal.parsing

import java.io.File

import org.greatcactus.vote.count.MainDataTypes.CandidateIndex
import org.greatcactus.vote.count.ballots.GroupInformation.GroupID
import org.greatcactus.vote.count.ballots._
import org.greatcactus.vote.count.ballots.parsing._
import org.greatcactus.vote.count.federal.DeducedAEC2013TicketSplits

import scala.collection.mutable.ArrayBuffer

object FederalElectionDataLoader2013 extends FederalElectionDataLoader("2013",false) {
  override def pageURL: String = "https://results.aec.gov.au/17496/Website/SenateDownloadsMenu-17496-csv.htm"
  override def downloadLocations : Array[String] = Array(pageURL,"Look under State below the line preferences for your state")
  def readOfficialResults2013(data:ElectionData) : AECDistributionOfPreferencesOfficial = readOfficialResults(data,17496,surnameFirst=false)
  def getCandidateInfo(state:String) : AECCandidateInformationSource = new AECCandidateInformationSource2013GroupTickets(rel("SenateGroupVotingTicketsDownload-17496.csv"))
  def getOtherInfo(state:String) : List[AECVoteSource] = List(new AECVoteSource2013ATL(rel("SenateUseOfGvtByGroupDownload-17496.csv")),getBTLInfo(state))
  override def getBTLInfo(state: String): AECBTLVoteSource = new AECVoteSource2013BTL(rel("SenateStateBtlDownload-17496-"+state+".zip"))
}

object FederalElectionDataLoader2016 extends FederalElectionDataLoader("2016",true) {
  override val pageURL = "https://results.aec.gov.au/20499/Website/SenateDownloadsMenu-20499-Csv.htm"
  def readOfficialResults2016(data:ElectionData) : AECDistributionOfPreferencesOfficial = readOfficialResults(data,20499,surnameFirst=true)
  def getCandidateInfo(state:String) : AECCandidateInformationSource = new AECCandidateInformationSource2016FirstPrefsByStateByVoteType(rel("SenateFirstPrefsByStateByVoteTypeDownload-20499.csv"))
  def getOtherInfo(state:String) : List[AECVoteSource] = List(getBTLInfo(state),new AEC2016OfficialResultsSource(rel("SenateSenatorsElectedDownload-20499.csv")))
  def getBTLInfo(state:String) = new AECVoteSource2016(rel("aec-senate-formalpreferences-20499-"+state+".zip"))

}

object FederalElectionDataLoader2019 extends FederalElectionDataLoader("2019",false) {
  override val pageURL = "https://tallyroom.aec.gov.au/SenateDownloadsMenu-24310-Csv.htm"
  def readOfficialResults2016(data:ElectionData) : AECDistributionOfPreferencesOfficial = readOfficialResults(data,24310,surnameFirst=true)

  def getCandidateInfo(state:String) : AECCandidateInformationSource = new AECCandidateInformationSource2019(rel("2019federalelection-all-candidates-nat-24-04-1643.csv"))
  def getOtherInfo(state:String) : List[AECVoteSource] = List(getBTLInfo(state))
  def getBTLInfo(state:String) = new AECVoteSource2019(rel("aec-senate-formalpreferences-24310-"+state+".zip"))
  override def dataAvailable(state: String): Boolean = rel("aec-senate-formalpreferences-24310-"+state+".zip").exists
}

class FederalElectionSpec(val state:String,val loader:FederalElectionDataLoader) extends ElectionSpecification {
  override def jurisdiction: String = "federal"
  override def year: String = loader.year
  override def region: String = state
  override def metadataAvailable: Boolean = true
  override def dataAvailable: Boolean = loader.dataAvailable(state)
  override def iterateOverRawBTLDataAvailable: Boolean = year!="2013"
  override def metadata: ElectionMetadata = if (dataAvailable) loader.loadJustMetadata(state) else loader.loadRawMetadata(state)
  override def data: ElectionData = loader.load(state)
  override def getIterateOverRawBTLData: IterateOverRawBTLData = loader.getIterator(state)
  override def ticketRoundingChoicesMade: Map[GroupID, CandidateIndex] = if (year=="2013") DeducedAEC2013TicketSplits.all(state)  else Map.empty
  override def numToBeElected: CandidateIndex = if (state=="ACT" || state=="NT") 2 else if (loader.doubleDissolution) 12 else 6
}


object FederalJurisdictionSpecification extends ElectionJurisdictionSpecification {
  override def jurisdiction: String = "federal"
  override def availableYears: Seq[String] = List("2013","2016","2019")

  override def getSpec(year: String): SetOfConcurrentElectionsSpecification = year match {
    case "2013" => FederalElectionDataLoader2013
    case "2016" => FederalElectionDataLoader2016
    case "2019" => FederalElectionDataLoader2019
  }
}


abstract class FederalElectionDataLoader(override val year:String,val doubleDissolution:Boolean) extends CachedElectionDataLoader("Federal/"+year) with SetOfConcurrentElectionsSpecification {
  def dataAvailable(state: String): Boolean = true
  override def jurisdiction:String = "federal"
  def availableRegions:Seq[String] = List("VIC","NSW","QLD","NT","SA","WA","TAS","ACT")
  def getSpec(region:String) : ElectionSpecification = new FederalElectionSpec(region,this)

  def pageURL : String
  def downloadLocations : Array[String] = Array(pageURL,"Look under formal preferences for your state")
  def readOfficialResults(data:ElectionData,electionNumber:Int,surnameFirst:Boolean) : AECDistributionOfPreferencesOfficial = {
    val zipfile = rel("SenateDopDownload-"+electionNumber+".zip")
    val counts = new ArrayBuffer[AECDistributionOfPreferencesOfficialSingleCount]
    var candidates = new Array[AECDistributionOfPreferencesOfficialSingleCountSingleCandidate](data.numCandidates)
    var exhausted : AECDistributionOfPreferencesOfficialSingleCountSingleCandidate = null
    for (cols<-CSVHelper.fromZipFile(zipfile,data.meta.electionName.electorate+".csv",1)) {
      val sc = new AECDistributionOfPreferencesOfficialSingleCountSingleCandidate(cols(9).toInt,cols(10).toInt,cols(11).toInt)
      val surname = cols(7)
      if (surname=="Exhausted") exhausted=sc
      else if (surname=="Gain/Loss") {
        counts+=new AECDistributionOfPreferencesOfficialSingleCount(candidates,exhausted,sc)
        candidates = new Array[AECDistributionOfPreferencesOfficialSingleCountSingleCandidate](data.numCandidates)
        exhausted=null
      } else {
        val givenname = cols(8)
        val name = if (surnameFirst) surname+", "+givenname else givenname+" "+surname
        //println("Found "+name)
        candidates(data.candidateIndexFromName(name))=sc
      }
    }
    new AECDistributionOfPreferencesOfficial(counts.toArray)
  }
  def loadRaw(state:String,candidatesSource:AECCandidateInformationSource,voteSources:AECVoteSource*) : ElectionData = {
    candidatesSource.read(state)
    val candidatesInfo = new AECCandidateInformation(candidatesSource)
    // deal with ATL group tickets
    val helper = new VoteInterpreter(candidatesInfo.groups,candidatesInfo.orderedCandidates.length)
    for (vs<-voteSources) vs.process(state, helper, candidatesInfo)
    val name = new ElectionName(year,"AEC","Federal",state)
    helper.getData(candidatesInfo.orderedCandidates,name,downloadLocations)
  }
  def getCandidateInfo(state:String) : AECCandidateInformationSource
  def getOtherInfo(state:String) : List[AECVoteSource]
  override def loadRaw(state:String) : ElectionData = {
    loadRaw(state,getCandidateInfo(state),getOtherInfo(state):_*)
  }
  def loadRawMetadata(state:String) : ElectionMetadata = {
    val info = getCandidateInfo(state)
    info.read(state)
    val parsed = new AECCandidateInformation(info)
    val electionName = new ElectionName(year,"AEC","Federal Senate",state)
    new ElectionMetadata(electionName,parsed.orderedCandidates,parsed.groups,None,downloadLocations)
  }
  def getBTLInfo(state:String) : AECBTLVoteSource
  def getIterator(state:String) : IterateOverRawBTLData = getBTLInfo(state).iterator(state,loadJustMetadata(state))

}

abstract class AECCandidateInformationSource {
    val candidates = new collection.mutable.HashMap[String,Candidate] // candidate_id to candidate
    val orderedCandidates = new collection.mutable.ArrayBuffer[Candidate]
    val groupTickets = new collection.mutable.HashMap[String,GroupTicketBuilder] 

    def read(state:String)    
}
class AECCandidateInformation(source:AECCandidateInformationSource) {
       val orderedCandidates: Array[Candidate] = source.orderedCandidates.toArray
       val rawgroups: Array[String] = orderedCandidates.map{_.group}.distinct.sorted(TicketNameSorter)
       val candidateToInt : Map[Candidate,CandidateIndex] = Map(orderedCandidates.zipWithIndex :_*)
       val candidateNameToInt : Map[String,CandidateIndex] = Map(orderedCandidates.map{_.name}.zipWithIndex :_*)
       val groups : Array[GroupInformation] = for (groupId<-rawgroups) yield source.groupTickets(groupId).toGroupInfo(candidateToInt)
       val groupIDToGroupNumber : Map[String,Int] = Map(rawgroups.zipWithIndex :_*)
       val candidates : Map[String,Candidate] = source.candidates.toMap
       /** From candidateID that means a group to the group ID */
       val candidateIDAsGroupVote : Map[String,String] = Map.empty++(for (gt<-source.groupTickets.values;candidateID<-gt.dummyIDForGroupVote) yield candidateID->gt.groupId)
}

class AECCandidateInformationSource2019(candidateFile:File) extends AECCandidateInformationSource {
  override def read(state: String): Unit = {
    var lastGroup : String = null
    for (cols<-CSVHelper(candidateFile,1)) if (cols(1)=="S" && cols(2)==state) {
      val name = cols(7)+" "+cols(6)
      val groupId = cols(4)
      orderedCandidates+=new Candidate(name,groupId,cols(5).toInt)
      if (lastGroup!=groupId) {
        lastGroup=groupId
        val groupName = cols(8)
        groupTickets+=groupId->new GroupTicketBuilder(groupId,groupName,None)
      }
    }
  }
}

/** The group tickets file also has enough information about all candidates and groups, said information not being present in the candidates file! */
class AECCandidateInformationSource2013GroupTickets(val groupVotingTickets:File) extends AECCandidateInformationSource {
  override def read(state:String) {
        for (cols<-CSVHelper(groupVotingTickets,2)) if (state==cols(0)) {
          val ownerTicket = cols(3)
          if (cols(4).startsWith("T")) println(cols.mkString(","))
          val ticketNo = cols(4).toInt
          val ticket = groupTickets.getOrElseUpdate(ownerTicket,{new GroupTicketBuilder(ownerTicket, cols(2),None)}) 
          val candidateID = cols(5)
          val candidate = candidates.getOrElseUpdate(candidateID,{
            val group = cols(6)
            groupTickets.getOrElseUpdate(group,{new GroupTicketBuilder(group, cols(11),None)}) 
            val surname = cols(7)
            val givenname = cols(8)
            val position = cols(9)
            val name = givenname+" "+surname
            val c = new Candidate(name,group,position.toInt)
            orderedCandidates+=c
            c
          })
          val preference = cols(12).toInt
          ticket.add(ticketNo,candidate, preference)
        }  
  }
}

/** The FirstPrefsByStateByVoteType file has enough information about all candidates and groups, said information not being present in the candidates file! */
class AECCandidateInformationSource2016FirstPrefsByStateByVoteType(val firstPrefsByStateByVoteType:File) extends AECCandidateInformationSource {
  // val dummyCandidateIDForGroup = new collection.mutable.HashMap[String,String] // candidate ID to Ticket letter 
  override def read(state:String) {
      for (cols<-CSVHelper(firstPrefsByStateByVoteType,2)) if (state==cols(0)) {
        val groupID = cols(1)
        val candidateID = cols(2)
        if (candidateID!="0") {
          val ticket = groupTickets.getOrElseUpdate(groupID,{new GroupTicketBuilder(groupID, cols(5),None)}) 
          val position = cols(3).toInt
          if (position==0) ticket.dummyIDForGroupVote = Some(candidateID) // dummyCandidateIDForGroup+= candidateID->groupID
          else candidates.getOrElseUpdate(candidateID,{
            val name = cols(4)
            val c = new Candidate(name,groupID,position)
            orderedCandidates+=c
            c
          })
        }
      }
  }
}

class AEC2016OfficialResultsSource(val officialResultsFile:File) extends AECVoteSource {
  override def process(state:String,helper:VoteInterpreter,candidates:AECCandidateInformation) {
    // deal with Group ticket votes
    val results = new collection.mutable.ArrayBuffer[Int]
    for (cols<-CSVHelper(officialResultsFile,2)) if (state==cols(0)) {
      val name = cols(2)+", "+cols(1)
      results+=candidates.candidateNameToInt(name)
    }
    helper.officialResults=Some(results.toArray)
  }
}

abstract class AECVoteSource {
  def process(state:String,helper:VoteInterpreter,candidates:AECCandidateInformation)
}

/** 2013 BTL format - in a zipped file */
class AECVoteSource2013BTL(btlZipedFile:File) extends AECBTLVoteSource {
  override def process(state:String,helper:VoteInterpreter,candidates:AECCandidateInformation) {
        // deal with BTL votes
    for (cols<-CSVHelper.fromZipFile(btlZipedFile, state+".csv",2)) {
      val prefstr = cols(1)
      if (prefstr!="") try {
        val candidate : Candidate = candidates.candidates(cols(0))
        val preferenceNumber : Int = cols(1).toInt
        if (preferenceNumber>0) {
          val paperid = cols(2).toInt*1000000000L+cols(3).toInt
          helper.addBTL(paperid, candidate, preferenceNumber)          
        }
      } catch { case _:NumberFormatException => }
    }

  }

  override def iterator(state: String, metadata: ElectionMetadata): IterateOverRawBTLData = ??? // Could write if sufficiently interested. If so, update FederalElectionSpec.iterateOverRawBTLDataAvailable

}

class AECVoteSource2013ATL(votesForGroup:File) extends AECVoteSource {
  override def process(state:String,helper:VoteInterpreter,candidates:AECCandidateInformation) {
        // deal with Group ticket votes
    for (cols<-CSVHelper(votesForGroup,2)) if (state==cols(0) && cols(1)!="UG") {
      val ticket = candidates.groupIDToGroupNumber(cols(1)) // letter
      val ticketVotes = cols(4).toInt // ATL ticket votes
      helper.addSATL(ticket,ticketVotes)
    }
  }
}


abstract class AECBTLVoteSource extends AECVoteSource {
  def iterator(state:String,metadata : ElectionMetadata) : IterateOverRawBTLData
}

/** The legislation for federal above the line counting changed significantly just before the 2016 election */

abstract class AECVoteSourceLike2016(prefZipedFile:File,numHeadLines:Int,requireVotesToHaveExactNumberOfColumns:Boolean,divisionColumn:Int,collectionPointColumn:Int) extends AECBTLVoteSource {

  def num(s:String) : Int = if (s=="/" || s=="*") 1 else s.toInt
  def getVotesFromCols(cols:Array[String]) : Array[String]

  override def process(state:String,helper:VoteInterpreter,candidates:AECCandidateInformation) {
    // deal with BTL votes
    val numGroups = candidates.rawgroups.length-(if (candidates.rawgroups.last=="UG") 1 else 0) // can't do an ATL vote for UG.
    val numCandidates = candidates.orderedCandidates.length
    var numInformalBTL = 0
    //var numInformal = 0
    for (cols<-CSVHelper.fromZipFile(prefZipedFile, state+".csv",numHeadLines)) {
      val votes = getVotesFromCols(cols)
      if (requireVotesToHaveExactNumberOfColumns && votes.length!=numGroups+numCandidates) println("Do not understand vote "+votes.mkString(",")+" found "+votes.length+" entries expecting "+numGroups+" ATL and "+numCandidates+" BTL.")
      else {
        val (atl,btl) = votes.splitAt(numGroups)
        val isBTL = btl.exists { ! _.isEmpty }
        val doneBTL = if (isBTL) { // Below the line
          val builder = new BelowTheLineBuilderByCandidateID(numCandidates)
          for (i<-btl.indices) {
            val v = btl(i)
            if (!v.isEmpty) builder.addVote(i,num(v))
          }
          val btlv = builder.get
          val isFormal = btlv.candidates.length>=6
          if (isFormal) helper.addBTL(builder.get) else numInformalBTL+=1
          isFormal
        } else false
        if (!doneBTL) {
          // Above the line
          val builder = new AboveTheLineBuilder(numGroups)
          for (i<-atl.indices) {
            val v = atl(i)
            if (!v.isEmpty) builder.addVote(candidates.rawgroups(i),num(v))
          }
          val atlv = builder.get
          if (!atlv.isEmpty) helper.addRATL(atlv) else helper.numInformal+=1
        }
      }
    }
    println("Found "+numInformalBTL+" failed attempts to vote below the line vs "+helper.extraBTLs.size+" successful. Total "+helper.numInformal+" informal.")
  }

  override def iterator(state:String,metadata : ElectionMetadata) : IterateOverRawBTLData = new IterateOverRawBTLData {
    override def meta : ElectionMetadata = metadata
    var rowsSoFar : Int = 0
    private var savedColumns : Array[String] = _

    def foreach(f:Array[String]=>Unit) {
      val numGroups = meta.groupInfo.length-(if (meta.groupInfo.last.groupId=="UG") 1 else 0) // can't do an ATL vote for UG.
      val numCandidates = meta.candidates.length
      rowsSoFar = 0
      for (cols<-CSVHelper.fromZipFile(prefZipedFile, state+".csv",numHeadLines)) {
        val votes = getVotesFromCols(cols)
        if (requireVotesToHaveExactNumberOfColumns && votes.length!=numGroups+numCandidates) println("Do not understand vote "+votes.mkString(",")+" found "+votes.length+" entries expecting "+numGroups+" ATL and "+numCandidates+" BTL.")
        else {
          val btl = votes.drop(numGroups)
          val isBTL = btl.exists { ! _.isEmpty }
          if (isBTL) {
            savedColumns = cols
            f(btl)
            rowsSoFar+=1
          }
        }
      }
      savedColumns=null
    }

    override def currentRowMetadata: Map[String, String] = Map("Electorate"->savedColumns(divisionColumn),"Collection Point"->savedColumns(collectionPointColumn))
  }

}

/** 2016 combined format - in a zipped file */
class AECVoteSource2016(prefZipedFile:File) extends AECVoteSourceLike2016(prefZipedFile,2,true,0,1) {
  val splitter = new Splitter

  def getVotesFromCols(cols: Array[String]): Array[String] = {
    val prefstr = cols(5) // contains a comma separated string containing written preferences, first for ATL then BTL.
    splitter.split(prefstr)
  }
}

/** 2019 combined format - in a zipped file */
class AECVoteSource2019(prefZipedFile:File) extends AECVoteSourceLike2016(prefZipedFile,1,false,1,2) {
  def getVotesFromCols(cols: Array[String]): Array[String] = {
    cols.drop(6) // first 6 columns metadata, next parties then candidates.
  }
}



/** Information on how votes were distributed, used to compare to my results */
class AECDistributionOfPreferencesOfficial(val counts:Array[AECDistributionOfPreferencesOfficialSingleCount])
class AECDistributionOfPreferencesOfficialSingleCount(val candidates:Array[AECDistributionOfPreferencesOfficialSingleCountSingleCandidate],val exhausted:AECDistributionOfPreferencesOfficialSingleCountSingleCandidate,val rounding:AECDistributionOfPreferencesOfficialSingleCountSingleCandidate)
class AECDistributionOfPreferencesOfficialSingleCountSingleCandidate(val papers:Int,val votesTrans:Int,val progressiveTotalVotes:Int)


/** Sort groups A-Z,AA-ZZ. Acts as a function that can be an arg to sorted */
object TicketNameSorter extends Ordering[String] {
  override def compare(x: String, y: String): Int = {
    val ldiff = x.length-y.length
    if (ldiff!=0) ldiff else x.compareTo(y)
  }
}

class GroupTicketBuilder(val groupId:String,val groupName:String,val groupShortName:Option[String]) {
  import collection.mutable
  val ticketbuilder = new mutable.HashMap[Int,mutable.HashMap[Int,Candidate]]
  var dummyIDForGroupVote : Option[String] = None
  def add(ticketNo:Int,candidate:Candidate,preference:Int) {
    val map = ticketbuilder.getOrElseUpdate(ticketNo,new mutable.HashMap[Int,Candidate])
    map+=preference->candidate
  }
  def toGroupInfo(candidateToInt:Map[Candidate,Int]) : GroupInformation = {
    def ticketFromMap(map:mutable.HashMap[Int,Candidate]) : Array[Int] = {
      val res = new Array[Int](map.size)
      for ((pref,candidate)<-map) res(pref-1)=candidateToInt(candidate)
      res
    }
    val tickets = ticketbuilder.toArray.sortBy(_._1).map{p=>ticketFromMap(p._2)}
    new GroupInformation(groupId,groupName,groupShortName,tickets)
  }
}

/*
class IterateOverRawBTLData2016(val meta:ElectionMetadata,iterator:CSVHelper,column:Int) extends IterateOverRawBTLData {
  var rowsSoFar : Int = 0
  private var savedColumns : Array[String] = null

  def foreach(f:Array[String]=>Unit) {
    val numGroups = meta.groupInfo.length-(if (meta.groupInfo.last.groupId=="UG") 1 else 0) // can't do an ATL vote for UG.
    val numCandidates = meta.candidates.length
    val splitter = new Splitter
    for (cols<-iterator) {
      val prefstr = cols(column) // contains a comma separated string containing written preferences, first for ATL then BTL.
      val votes = splitter.split(prefstr)
      if (votes.length!=numGroups+numCandidates) throw new IOException("Internal error. Do not understand vote "+prefstr+" found "+votes.length+" entries expecting "+numGroups+" ATL and "+numCandidates+" BTL.")
      else {
        val (_,btl) = votes.splitAt(numGroups)
        val isBTL = btl.exists { ! _.isEmpty }
        if (isBTL) {
          savedColumns = cols
          f(btl)
          rowsSoFar+=1
        }
      }
    }
  }

  override def currentRowMetadata: Map[String, String] = Map("Electorate"->savedColumns(0),"Collection Point"->savedColumns(1))
}
*/
/*
object FederalIterateOverRawBTLData {
  def get(year:String,state:String) : IterateOverRawBTLData = {
    val meta = FederalElectionData.loadRawMetadata(year,state)
    year match {
      case "2016" => FederalElectionDataLoader2016.getIterator(state)  // new IterateOverRawBTLData2016(meta,CSVHelper.fromZipFile(FederalElectionDataLoader2016.rel("aec-senate-formalpreferences-20499-"+state+".zip"), state+".csv",2),5)
      case "2019" => FederalElectionDataLoader2019.getIterator(state)
      case _ => throw new IOException("No preference data available for "+year)
    }
  }
}
*/
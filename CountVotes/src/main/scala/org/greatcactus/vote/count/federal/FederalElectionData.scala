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

object FederalElectionData {

  def load2013(state:String) = {
    val cache = new File(IOUtil.baseDir,"Cache/Federal/2013/"+state+".txt")
    try {
      ElectionDataFastIO.loadPickled(cache)
    } catch { 
      case e:Exception => 
        // e.printStackTrace()
        println("Loading manually"); 
        val res = loadRaw2013(state)
        ElectionDataFastIO.savePickled(res,cache)
        res 
    }
  }
  def loadRaw2013(state:String) = {
    val sourcedir = new File(IOUtil.baseDir,"Elections/Federal/2013")
    def rel(f:String) = new File(sourcedir,f)
    loadRaw(state,"2013",new AECCandidateInformationSource2013GroupTickets(rel("SenateGroupVotingTicketsDownload-17496.csv")),new AECVoteSource2013ATL(rel("SenateUseOfGvtByGroupDownload-17496.csv")),new AECVoteSource2013BTL(rel("SenateStateBtlDownload-17496-"+state+".zip")))
  }
  def load2016(state:String) = {
    val cache = new File(IOUtil.baseDir,"Cache/Federal/2016/"+state+".txt")
    try {
      ElectionDataFastIO.loadPickled(cache)
    } catch { 
      case e:Exception => 
        // e.printStackTrace()
        println("Loading manually"); 
        val res = loadRaw2016(state)
        ElectionDataFastIO.savePickled(res,cache)
        res 
    }
  }
  def load2016Metadata(state:String) : ElectionMetadata = {
    val cache = new File(IOUtil.baseDir,"Cache/Federal/2016/"+state+".txt")
    try {
      ElectionDataFastIO.loadPickledMetadata(cache)
    } catch {
      case e:Exception =>
        // e.printStackTrace()
        println("Loading manually");
        val res = loadRaw2016(state)
        ElectionDataFastIO.savePickled(res,cache)
        res.meta
    }
  }
  def loadRaw2016(state:String) = {
    val sourcedir = new File(IOUtil.baseDir,"Elections/Federal/2016")
    def rel(f:String) = new File(sourcedir,f)
    loadRaw(state,"2016",new AECCandidateInformationSource2016FirstPrefsByStateByVoteType(rel("SenateFirstPrefsByStateByVoteTypeDownload-20499.csv")),new AECVoteSource2016(rel("aec-senate-formalpreferences-20499-"+state+".zip")),new AEC2016OfficialResultsSource(rel("SenateSenatorsElectedDownload-20499.csv")))
  }

  def loadRaw(state:String,year:String,candidatesSource:AECCandidateInformationSource,voteSources:AECVoteSource*) : ElectionData = {
    candidatesSource.read(state)
    val candidatesInfo = new AECCandidateInformation(candidatesSource)
    // deal with ATL group tickets
    val helper = new VoteInterpreter(candidatesInfo.groups,candidatesInfo.orderedCandidates.length)
    for (vs<-voteSources) vs.process(state, helper, candidatesInfo) 
    helper.getData(candidatesInfo.orderedCandidates,state,year)
  }

  def readOfficialResults2013(data:ElectionData) : AECDistributionOfPreferencesOfficial = readOfficialResults(data,17496,false)
  def readOfficialResults2016(data:ElectionData) : AECDistributionOfPreferencesOfficial = readOfficialResults(data,20499,true)

  def readOfficialResults(data:ElectionData,electionNumber:Int,surnameFirst:Boolean) : AECDistributionOfPreferencesOfficial = {
    val zipfile = new File(IOUtil.baseDir,"Elections/Federal/"+data.year+"/SenateDopDownload-"+electionNumber+".zip")
    val counts = new ArrayBuffer[AECDistributionOfPreferencesOfficialSingleCount]
    var candidates = new Array[AECDistributionOfPreferencesOfficialSingleCountSingleCandidate](data.numCandidates)
    var exhausted : AECDistributionOfPreferencesOfficialSingleCountSingleCandidate = null
    for (cols<-CSVHelper.fromZipFile(zipfile,data.name+".csv",1)) {
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
}

abstract class AECCandidateInformationSource {
    val candidates = new collection.mutable.HashMap[String,Candidate] // candidate_id to candidate
    val orderedCandidates = new collection.mutable.ArrayBuffer[Candidate]
    val groupTickets = new collection.mutable.HashMap[String,GroupTicketBuilder] 

    def read(state:String)    
}
class AECCandidateInformation(source:AECCandidateInformationSource) {
       val orderedCandidates = source.orderedCandidates.toArray
       val rawgroups = orderedCandidates.map{_.group}.toSet.toArray.sorted(TicketNameSorter)
       val candidateToInt : Map[Candidate,Int] = Map.empty++(orderedCandidates.zipWithIndex)
       val candidateNameToInt : Map[String,Int] = Map.empty++(orderedCandidates.map{_.name}.zipWithIndex)
       val groups : Array[GroupInformation] = for (groupId<-rawgroups) yield source.groupTickets(groupId).toGroupInfo(candidateToInt)
       val groupIDToGroupNumber : Map[String,Int] = Map.empty++(rawgroups.zipWithIndex)
       val candidates : Map[String,Candidate] = source.candidates.toMap
       /** From candidateID that means a group to the group ID */
       val candidateIDAsGroupVote : Map[String,String] = Map.empty++(for (gt<-source.groupTickets.values;candidateID<-gt.dummyIDForGroupVote) yield candidateID->gt.groupId)
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
class AECVoteSource2013BTL(btlZipedFile:File) extends AECVoteSource {
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

/** 2016 combined format - in a zipped file */
class AECVoteSource2016(prefZipedFile:File) extends AECVoteSource {
  def num(s:String) = if (s=="/" || s=="*") 1 else s.toInt
  override def process(state:String,helper:VoteInterpreter,candidates:AECCandidateInformation) {
        // deal with BTL votes
    val numGroups = candidates.rawgroups.length-(if (candidates.rawgroups.last=="UG") 1 else 0) // can't do an ATL vote for UG.
    val numCandidates = candidates.orderedCandidates.length
    val splitter = new Splitter
    var numInformalBTL = 0
    var numInformal = 0
    for (cols<-CSVHelper.fromZipFile(prefZipedFile, state+".csv",2)) {
      val prefstr = cols(5) // contains a comma separated string containing written preferences, first for ATL then BTL.
      val votes = splitter.split(prefstr)
      if (votes.length!=numGroups+numCandidates) println("Do not understand vote "+prefstr+" found "+votes.length+" entries expecting "+numGroups+" ATL and "+numCandidates+" BTL.")
      else {
        val (atl,btl) = votes.splitAt(numGroups)
        val isBTL = btl.exists { ! _.isEmpty }   
        val doneBTL = if (isBTL) { // Below the line
          val builder = new BelowTheLineBuilderByCandidateID(numCandidates)
          for (i<-0 until numCandidates) {
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
          for (i<-0 until numGroups) {
            val v = atl(i)
            if (!v.isEmpty) builder.addVote(candidates.rawgroups(i),num(v))
          }
          val atlv = builder.get
          if (!atlv.isEmpty) helper.addRATL(atlv) else numInformal+=1
        }
      }
    }
    println("Found "+numInformalBTL+" failed attempts to vote below the line vs "+helper.extraBTLs.size+" successful. Total "+numInformal+" informal.")
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
  import collection.mutable.HashMap
  val ticketbuilder = new HashMap[Int,HashMap[Int,Candidate]]
  var dummyIDForGroupVote : Option[String] = None
  def add(ticketNo:Int,candidate:Candidate,preference:Int) {
    val map = ticketbuilder.getOrElseUpdate(ticketNo,new HashMap[Int,Candidate])
    map+=preference->candidate
  }
  def toGroupInfo(candidateToInt:Map[Candidate,Int]) : GroupInformation = {
    def ticketFromMap(map:HashMap[Int,Candidate]) : Array[Int] = {
      val res = new Array[Int](map.size)
      for ((pref,candidate)<-map) res(pref-1)=candidateToInt(candidate)
      res
    }
    val tickets = ticketbuilder.toArray.sortBy(_._1).map{p=>ticketFromMap(p._2)}
    new GroupInformation(groupId,groupName,groupShortName,tickets)
  }
}


object CSVHelper {
  def fromIS(stream:InputStream,headingLines:Int,separator:Char=',',charEncoder:String="UTF8") = new CSVHelper(stream,headingLines,separator,charEncoder)
  def apply(file:File,headingLines:Int,separator:Char=',',charEncoder:String="UTF8") = new CSVHelper(new FileInputStream(file),headingLines,separator,charEncoder)
  def fromZipFile(zipedFile:File,innerfilename:String,headingLines:Int) = {
    val zipFile = new ZipFile(zipedFile)
    import collection.JavaConverters._
    zipFile.entries().asScala.find { _.getName.contains(innerfilename) } match {
          case Some(entry) => new CSVHelper(zipFile.getInputStream(entry),headingLines)
          case None => throw new Exception("Could not find "+innerfilename+" in "+zipedFile)
        }
  }
}
class CSVHelper(stream:InputStream,headingLines:Int,separator:Char=',',charEncoder:String="UTF8") {
  val reader = new BufferedReader(new InputStreamReader(stream,charEncoder))
  val headings = for (i<-0 until headingLines) yield reader.readLine()
  val splitter = new Splitter(separator)
  def splitHeading(headingNumber:Int) : Array[String] = splitter.split(headings(headingNumber))
  var lineCount = headingLines
  //val headings = reader.readLine.split(separator)
  def foreach(f:Array[String]=>Unit) {
    var line = reader.readLine()
    try {
      while (line!=null) {
        lineCount+=1
        // val split = line.split(separator)
        f(splitter.split(line))
        line = reader.readLine()
      }
      reader.close()
    } catch { case e:Exception => println("Error reading line number "+lineCount+" : "+line); throw e; }
  } 
}
/** Non thread safe helper to split a line (e.g. csv) into components, allowing for quotes */
class Splitter(separator:Char=',') {
    val split = new ArrayBuffer[String]
    val sb = new StringBuilder

    /** Non-thread safe */
    def split(line:String) : Array[String] = {
      var inquotes = false
      for (c<-line) {
        if (c=='"') inquotes= !inquotes
        else if (c==','&& !inquotes) { split+=sb.toString; sb.clear() }
        else sb+=c
      }
      split+=sb.toString; sb.clear()
      val res = split.toArray
      split.clear()
      res
    }
}

/*
    Copyright 2020 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count.act.parsing

import java.io.FileInputStream
import java.util

import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.ss.usermodel.Workbook
import org.greatcactus.vote.count.MainDataTypes.CandidateIndex
import org.greatcactus.vote.count.ballots.GroupInformation.GroupID
import org.greatcactus.vote.count.ballots.{BTL, CachedElectionDataLoader, Candidate, ElectionData, ElectionMetadata, ElectionName, ElectionSpecification, GroupInformation, SetOfConcurrentElectionsSpecification}
import org.greatcactus.vote.count.ballots.parsing.{BTLCandidateList, CSVHelper, IterateOverRawBTLData}

import scala.collection.mutable.ArrayBuffer
import org.apache.poi.xssf.usermodel.{XSSFRow, XSSFWorkbook}

object ACTElectionData extends App {

  val loader = new ACTElectionDataLoader("2016")
  for (electorate<-loader.availableRegions) {
    println("Loading "+electorate)
    val data = loader.loadRaw(electorate)
    data.printStatus()
  }

}


class ACTElectionSpec(val state:String,val loader:ACTElectionDataLoader, override val numToBeElected:Int) extends ElectionSpecification {
  override def jurisdiction: String = "ACT"
  override def year: String = loader.year
  override def region: String = state
  override def metadataAvailable: Boolean = true
  override def dataAvailable: Boolean = loader.dataAvailable(state)
  override def iterateOverRawBTLDataAvailable: Boolean = false
  override def metadata: ElectionMetadata = if (dataAvailable) loader.loadJustMetadata(state) else loader.loadRawMetadata(state)
  override def data: ElectionData = loader.load(state)
  override def getIterateOverRawBTLData: IterateOverRawBTLData = throw new Exception("Not available")
  override def ticketRoundingChoicesMade: Map[GroupID, CandidateIndex] = Map.empty
}

class ACTElectionDataLoader(override val year:String) extends CachedElectionDataLoader("ACT/"+year) with SetOfConcurrentElectionsSpecification {
  def dataAvailable(state: String): Boolean = true
  override def jurisdiction:String = "ACT"

    /** map from electorate ids (ecode) to electorate names */
  private lazy val electorates : Map[String,String] = {
    val csvFile = CSVHelper.apply(rel("Electorates.txt"),1)
    var res : Map[String,String] = Map.empty
    for (line<-csvFile) if (line.length==2) res+=line(0)->line(1)
    res
  }
  private lazy val electorate_to_ecode : Map[String,String] = electorates.map{case (ecode,name) => name->ecode}

  /** Load in the parties for some electorate code */
  private def parties(ecode:String) : Array[GroupInformation] = {
    val csvFile = CSVHelper.apply(rel("Groups.txt"),1)
    var res =new ArrayBuffer[GroupInformation]()
    for (line<-csvFile) if (line.length==5 && line(0)==ecode) res+=new GroupInformation(line(1),line(2),Some(line(3)),Array())
    res.toArray
  }
  /** Load in the candidates for some electorate code */
  private def candidates(ecode:String) : Array[Candidate] = {
    val csvFile = CSVHelper.apply(rel("Candidates.txt"),1)
    var res =new ArrayBuffer[Candidate]()
    for (line<-csvFile) if (line.length==4 && line(0)==ecode) res+=new Candidate(line(3),line(1),line(2).toInt+1)
    res.toArray
  }

  def loadRawMetadata(state:String) : ElectionMetadata = {
    val ecode = electorate_to_ecode(state)
    val electionName = new ElectionName(year,"Elections ACT","ACT",state)
    new ElectionMetadata(electionName,candidates(ecode),parties(ecode),None,downloadLocations)
  }

  def availableRegions:Seq[String] = electorates.values.toList
  def getSpec(region:String) : ElectionSpecification = new ACTElectionSpec(region,this,if (region=="Molonglo") 7 else 5)

  def pageURL : String = "https://www.elections.act.gov.au/elections_and_voting/past_act_legislative_assembly_elections/"+year+"-election"
  def downloadLocations : Array[String] = Array(pageURL)

  private def firstNameFirst(surnameFirst:String) : String = {
    val components = surnameFirst.split(',').map{_.trim}
    components(1)+" "+components(0)
  }
  def readOfficialResults(metadata: ElectionMetadata) : ACTOfficialDistributionOfPreferences = {
    val candidateNames : Array[String] = metadata.candidates.map(c=>firstNameFirst(c.name))
    // println(candidateNames.mkString(","))
    val numCandidates = candidateNames.length
    val candidateIndexOfName: Map[String, CandidateIndex] = Map(candidateNames.zipWithIndex :_*)
    def isDOPFileName(filename:String) : Boolean = { filename.startsWith("table2") && (filename.contains(metadata.electionName.electorate)||filename.contains(metadata.electionName.electorate.toLowerCase))}
    val file = rel("Distribution Of Preferences").listFiles().find(f=>isDOPFileName(f.getName)).get
    // val fis = new FileInputStream(file)
    val workbook : Workbook = if (file.getName.endsWith("xlsx")) new XSSFWorkbook(file) else new HSSFWorkbook(new FileInputStream(file))
    val sheet = workbook.getSheetAt(0)
    val rowIndexForNames = if (file.getName.endsWith("xlsx")) 2 else 1
    val namesRow = sheet.getRow(rowIndexForNames)
    val columnForCandidate = new Array[Int](numCandidates)
    val countColumn = 0
    def cleanName(name:String) : String = { // 2008 candidate names are preceded by "party,"
      val comma = name.indexOf(',')
      if (comma== -1) name else name.substring(comma+1)
    }
    for (col<-countColumn+1 to countColumn+numCandidates) {
      columnForCandidate(candidateIndexOfName(cleanName(namesRow.getCell(col).getStringCellValue).trim))=col
    }
    val columnExhaused = countColumn+numCandidates+1
    val columnLoss = columnExhaused+1
    val columnRemark = columnLoss+2
    val counts = new ArrayBuffer[ACTOfficialDistributionOfPreferencesCount]
    var rowIndex = rowIndexForNames+1
    def parseRow(row:org.apache.poi.ss.usermodel.Row,elected:ArrayBuffer[CandidateIndex]) : ACTOfficialDistributionOfPreferencesLine = {
      def num(col:Int) : Double = {
        val cell = row.getCell(col)
        if (cell==null) 0 else cell.getNumericCellValue
      }
      val tallys = columnForCandidate.map{num}
      val exhausted = num(columnExhaused)
      val lost = num(columnLoss)
      val remark = row.getCell(columnRemark).getStringCellValue
      val finalRemark = if (remark.isEmpty) "" else {
        val remarks = remark.split('.').map{_.trim}
        var use = ""
        for (r<-remarks) if (r.nonEmpty) {
          val whoElected = candidateNames.indexWhere{n=>r.contains(n+" elected")}
          if (whoElected== -1) use=r+"." else elected+=whoElected
        }
        use
      }
      val whoRemarked = candidateNames.indexWhere{remark.startsWith}
      new ACTOfficialDistributionOfPreferencesLine(tallys,exhausted,lost,finalRemark,if (whoRemarked== -1) None else Some(whoRemarked))
    }
    def lookForRow(): Boolean = {
      val rowDelta = sheet.getRow(rowIndex)
      val rowSum = sheet.getRow(rowIndex+1)
      if (rowDelta==null || rowSum==null) false else {
        val countCell = rowDelta.getCell(countColumn) // before 2020 this contains the count
        val countCell2 = rowSum.getCell(countColumn) // 2020 and later this contains the count
        if ((countCell==null || countCell.getNumericCellValue == 0.0)&&(countCell2==null || countCell2.getNumericCellValue == 0.0)) false else {
          val elected = new ArrayBuffer[CandidateIndex]
          val delta = parseRow(rowDelta,elected)
          var end = parseRow(sheet.getRow(rowIndex+1),elected)
          if (counts.isEmpty && !util.Arrays.equals(delta.tallys,end.tallys)) { rowIndex-=1; end=delta; } // before 2020, first row contains identical deltas and results. 2020 onwards, first line is suppressed.
          counts+=new ACTOfficialDistributionOfPreferencesCount(delta,end,elected.toArray)
          true
        }
      }
    }
    while (lookForRow()) { rowIndex+=2; }
    new ACTOfficialDistributionOfPreferences(counts.toArray)
  }

  override def loadRaw(state:String) : ElectionData = {
    val metadata = loadRawMetadata(state)
    val candidate_of_pcodeccode : Map[(String,String),CandidateIndex] = Map(metadata.candidates.map(c=>(c.group,(c.position-1).toString)).zipWithIndex :_*)
    val csvFile = CSVHelper.apply(rel(state+"Total.txt"),1)
    val btls = new ArrayBuffer[BTL]
    val prefs = new ArrayBuffer[CandidateIndex]()
    var lastPaper : Option[String] = None
    def addPaper(): Unit = {
      if (lastPaper.isDefined) {
        btls+=new BTL(prefs.toArray,1)
        prefs.clear()
      }
    }
    for (line<-csvFile) if (line.length==6) {
      val pref = line(2).toInt // 1,2,...
      val candidate = candidate_of_pcodeccode((line(3),line(4)))
      val paper = line(1)
      if (!lastPaper.contains(paper)) { addPaper(); lastPaper=Some(paper) }
      if (pref!=prefs.length+1) throw new Exception("Preferences not in order. No reason they should be other than they seem to be, but it saves work if we assume they are and this is a safety check")
      prefs+=candidate
    }
    addPaper()
    val cleanBtls = BTLCandidateList.findMultiples(btls.toArray)
    new ElectionData(metadata,Array(),Array(),cleanBtls,0)
  }

}

/** Official DOP for one count */
class ACTOfficialDistributionOfPreferencesLine(val tallys:Array[Double],val exhausted:Double,val lossByFraction:Double,val remarks:String,val whoRemarked:Option[CandidateIndex])
class ACTOfficialDistributionOfPreferencesCount(val delta:ACTOfficialDistributionOfPreferencesLine,val end:ACTOfficialDistributionOfPreferencesLine,val elected:Array[CandidateIndex])

class ACTOfficialDistributionOfPreferences(val counts:Array[ACTOfficialDistributionOfPreferencesCount])
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


package org.greatcactus.vote.count.act

import java.io.File

import org.greatcactus.vote.count.MainDataTypes.CandidateIndex
import org.greatcactus.vote.count.{ElectionReport, ReportSaverDirectory}
import org.greatcactus.vote.count.act.parsing.ACTElectionDataLoader
import org.greatcactus.vote.count.ballots.ElectionCountRules

object RunACTElection extends App {
  val printDebugMessages:Boolean = true
  val mainReportDir = new File("ACT2020Reports")

  val actData = new ACTElectionDataLoader("2020")

  val computeMargins = true

  for (region<-actData.availableRegions) {
    val data = actData.load(region)
    val reportDir = new ReportSaverDirectory(new File(mainReportDir,data.meta.electionName.electorate))
    data.printStatus()
    val ecDeemedOrder:Seq[Int] = List.empty
    val roundTo6DecimalDigits = true
    val worker = new ACTElectionHelper (data,5,ecDeemedOrder,printDebugMessages,Set.empty,if (roundTo6DecimalDigits) 1000000 else 1,false)
    worker.run(if (computeMargins) Some(reportDir) else None)
    ElectionReport.saveReports(reportDir,worker.report,data.meta)
  }

}

object RunACTElectionsComparingBuggyVsOldCold extends App {
  val printDebugMessages:Boolean = false
  val roundTo6DecimalDigits = false

  for (year<- List("2008","2012","2016","2020")) {
    println("***** Year ***** "+year)
    val actData = new ACTElectionDataLoader(year)
    for (region<-actData.availableRegions) {
      val data = actData.load(region)
      val ecDeemedOrder:Seq[Int] = List.empty
      val officialWinners = actData.readOfficialResults(data.meta).counts.flatMap(_.elected).toList
      def winners(emulateACT2020behaviour:Boolean): List[CandidateIndex]  = {
        val worker = new ACTElectionHelper (data,officialWinners.length,ecDeemedOrder,printDebugMessages,Set.empty,if (roundTo6DecimalDigits) 1000000 else 1,emulateACT2020behaviour)
        worker.run(None)
        worker.report.electedCandidates.toList
      }
      val winnersWithout2020bugs: List[CandidateIndex] = winners(false)
      val winnersWith2020bugs: List[CandidateIndex] = winners(true)
      val differ = winnersWith2020bugs!=winnersWithout2020bugs || winnersWith2020bugs!=officialWinners
      println((if (differ) "!!!!" else "    ")+" "+year+" "+region+" "+winnersWithout2020bugs+" "+winnersWith2020bugs+" "+officialWinners)
    }
  }

}

object ACT2016Rules extends ElectionCountRules {
  override val name: String = "ACT2016"
  override val usedIn: List[String] = List("ACT 2012","ACT 2016")
  override val minATLmarksToBeValid: Int = 0 // NA
  override val minBTLmarksToBeValid: Int = 1
}

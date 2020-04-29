/*
    Copyright 2017-2020 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count.vic

import java.io.File

import org.greatcactus.vote.count.ElectionReport
import org.greatcactus.vote.count.ballots.ElectionCountRules
import org.greatcactus.vote.count.vic.parsing.Vic2014Data

object RunVicElection extends App {
  val printDebugMessages:Boolean = true
  val reportDir = new File("Vic2014Reports")
  
  for (region<-Vic2014Data.regions) {
    val data = Vic2014Data.load(region)
    data.printStatus()
    val ticketRoundingChoices:Map[String,Int] = Map.empty
    val ecDeemedOrder:Seq[Int] = List.empty // WA2017OfficialResults.usedOrders.getOrElse(region.data.meta.electionName.electorate,List.empty)
    val worker = new VicElectionHelper (data,5,ticketRoundingChoices,ecDeemedOrder,printDebugMessages,Set.empty)
    worker.run()
    ElectionReport.saveReports(new File(reportDir,data.meta.electionName.electorate),worker.report,data.meta)
  }
  
}

object VictoriaSenate2014Rules extends ElectionCountRules {
  override val name: String = "Victoria2014"
  override val usedIn: List[String] = List("Victoria 2014")
  override val minATLmarksToBeValid: Int = 0 // NA
  override val minBTLmarksToBeValid: Int = 5
}

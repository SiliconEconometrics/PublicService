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

package org.greatcactus.vote.count.wa

import java.io.File

import org.greatcactus.vote.count.ElectionReport
import org.greatcactus.vote.count.wa.parsing.{WA2017ElectionData, WA2017OfficialResults}

object RunWAElection extends App {
  val printDebugMessages:Boolean = true
  val reportDir = new File("WA2016Reports")
  
  val regions = WA2017ElectionData.loadAllRaw()
  for (region<-regions) {
    region.data.printStatus()
    println("Enrolment "+region.enrolment)
    //for (g<-region.data.groupInfo) println(g.line)
    val ticketRoundingChoices:Map[String,Int] = Map.empty
    val ecDeemedOrder:Seq[Int] = WA2017OfficialResults.usedOrders.getOrElse(region.data.meta.electionName.electorate,List.empty)
    val worker = new WAElectionHelper (region.data,region.vacancies,ticketRoundingChoices,ecDeemedOrder,printDebugMessages,Set.empty)
    worker.run()
    ElectionReport.saveReports(new File(reportDir,region.data.meta.electionName.electorate),worker.report,region.data.meta)
  }
  
}
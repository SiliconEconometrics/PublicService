/*
    Copyright 2017 Silicon Econometrics Pty. Ltd.

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

object RunWAElection extends App {
  val printDebugMessages:Boolean = true
  val reportDir = new File("WA2016Reports")
  
  val regions = WAElectionData.loadAllRaw()
  for (region<-regions) {
    region.data.printStatus()
    println("Enrolment "+region.enrolment)
    //for (g<-region.data.groupInfo) println(g.line)
    val ticketRoundingChoices:Map[String,Int] = Map.empty
    val ecDeemedOrder:Seq[Int] = List()
    val worker = new WAElectionHelper (region.data,region.vacancies,ticketRoundingChoices,ecDeemedOrder,printDebugMessages,Set.empty)
    worker.run()
    ElectionReport.saveReports(new File(reportDir,region.data.name),worker.report,region.data)
  }
  
}
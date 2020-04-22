package org.greatcactus.vote.count.wa

import java.io.File

import org.greatcactus.vote.count.ElectionReport
import org.greatcactus.vote.count.wa.parsing.WA2013ElectionData

object WA2013 extends App {
  val reportDir = new File("WA2013Reports")

  val (data,officialResult) = WA2013ElectionData.loadRegionRaw("Smet")
  data.printStatus()
      val ticketRoundingChoices:Map[String,Int] = Map.empty
    val ecDeemedOrder:Seq[Int] = List()
    val worker = new WAElectionHelper (data,officialResult.vacancies,ticketRoundingChoices,ecDeemedOrder,false,Set.empty)
    worker.run()
    ElectionReport.saveReports(new File(reportDir,data.meta.electionName.shortFileName),worker.report,data.meta)

}

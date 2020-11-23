/*
    Copyright 2015-2020 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count.nsw
import java.io.File

import org.greatcactus.vote.count._
import org.greatcactus.vote.count.ballots.ElectionData

import scala.util.Random


class NSWProbabilisticRun(numTotalRuns:Int,votedata:ElectionData,numCandidatesToElect:Int,electionRules:ElectionRules,ineligibleCandidates:Set[Int],reportLocations:Option[StochasticReportOptions],officialDOP : Option[NSWWholeCountSummary]=None)
  extends ProbabilisticRunner(reportLocations) {
  override def doOneRun(random:Random) : ElectionResultReport = {
    val worker = new NSWElectionHelper(votedata,numCandidatesToElect,random,electionRules,ineligibleCandidates)
    worker.run()
    worker.report
  }

  val dist: NSWDistributionSummary = new NSWDistributionSummary(votedata.numCandidates)
  private var numExtremenessRunsDone: Int = 0

  override def addStochiasticReports(report:ElectionResultReport) {
    if (officialDOP.isDefined) {
      val summary = NSWWholeCountSummary.ofReport(report)
      this.synchronized {
        if (numExtremenessRunsDone*2<numTotalRuns) dist.addJustGenerateFunction(summary)
        else dist.addJustEvaluateFunctionDistribution(summary)
        numExtremenessRunsDone+=1
      }
    }
    super.addStochiasticReports(report)
  }


}



object ProbabilisticWork {
  private val pendingExtremenessReports = new collection.mutable.ArrayBuffer[(String,Int,Double)]
  private lazy val extremenessDir: File = {
    val dir = new File("NSWExtremenessReports")
    if (dir.exists()) dir.listFiles().foreach { _.delete()}
    dir.mkdirs()
    ElectionReport.createCSS(dir)
    dir
  }
  def writeExtremenessSummary() {
    val xml =
      <html>
        <head>
          <meta charset="UTF-8"/>
          <title>Official vs. distribution Summary</title>
          <link href="report.css" type="text/css" rel="stylesheet"></link>
        </head>
        <body>
          <table class="Display">
            <tr class="Head"><th>Contest</th><th>Statistic</th><th>P Value</th></tr>
            {
            for (r<-pendingExtremenessReports) yield <tr class="Striped"><td><a href={r._1+".html"}>{r._1}</a></td><td>{r._2}</td><td>{r._3}</td></tr>
            }
          </table>
        </body>
      </html>
    scala.xml.XML.save(new File(extremenessDir,"About.html").toString,xml)
  }


  def runProbabilisticly(numTotalRuns:Int,numThreads:Int,votedata:ElectionData,printProgress:Boolean,numCandidatesToElect:Int,electionRules:ElectionRules,output:Option[StochasticStatusOutput],ineligibleCandidates:Set[Int],reportLocations:Option[StochasticReportOptions],officialDOP : Option[NSWWholeCountSummary]=None): Unit = {
    if (output.isEmpty) votedata.printStatus()
    val runner = new NSWProbabilisticRun(numTotalRuns,votedata,numCandidatesToElect,electionRules,ineligibleCandidates,reportLocations,officialDOP)
    runner.runProbabilisticly(votedata.meta,numTotalRuns,numThreads,printProgress,output)
    if (officialDOP.isDefined) {

      val (extremeness,pValue) = runner.dist.extremeness(officialDOP.get)
      val name = votedata.meta.electionName.shortPrintName
      pendingExtremenessReports+= ((name,extremeness,pValue))
      println(name+" Official DOP extremeness "+extremeness+" pValue="+pValue)
      scala.xml.XML.save(new File(extremenessDir,name+".html").toString, runner.dist.extremenessReport(officialDOP.get, votedata.candidates.map{_.name}, name))

      //if (pValue<0.1) {
      //   dist.printoutDetailedExtremeness(officialDOP.get,votedata.candidates.map{_.name})
      //if (pValue<0.01) System.exit(0)
      //}
    }
  }
}


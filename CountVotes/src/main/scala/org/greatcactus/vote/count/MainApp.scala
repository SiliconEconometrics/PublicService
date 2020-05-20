/*
    Copyright 2016-2020 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count

import java.io.File

import org.greatcactus.vote.count.MainDataTypes.CandidateIndex
import org.greatcactus.vote.count.ballots.{ElectionCountRules, ElectionData, ElectionDataFastIO, OCRError}
import org.greatcactus.vote.count.federal.{FederalSenateCount, FederalSenateCountHelper, FederalSenate2013Rules, FederalSenate2016Rules}
import org.greatcactus.vote.count.margin.ElectionChanged
import org.greatcactus.vote.count.vic.VictoriaSenate2014Rules

import scala.util.Random

object MainApp extends App {
  if (args.length==0) println(
    """
      | Allowable arguments
      | --stv <file>               specify file of election information, in Andrew stv format. You can get format and data from vote.andrewconway.org
      | --out <dirname>            specify directory to write output (full distribution of preferences) to. Will be created if not currently existing. Default is a subdirectory of current directory based on name of election in stv file.
      | --rules rulename           specify rules used for running the election. Default "federal".
      | --NumSeats number          specify number of candidates to elect (default 6).
      | --ECOrder candidatelist    specify a comma separated list of candidate numbers that define the order the EC breaks ties.
      | --exclude candidatelist    specify a comma separated list of candidate numbers to be deemed ineligible
      | --prohibitMultipleEliminations                           For federal elections, don't use section 13(A) multiple eliminations. AEC didn't use this in 2016 and 2019.
      | --finishExclusionEvenIfAllWillBeElected                  Don't stop an exclusion just because the election is over. AEC did this in 2016. Doesn't affect who is elected, only reports.
      | --finishSuplusDistributionEvenIfEveryoneWillGetElected   Don't stop an surplus distribution just because the election is over. AEC did this in 2016. Doesn't affect who is elected, only reports
      | --interruptExclusionAtStartOfExclusionIfAllWillBeElected Do stop an exclusion before even the first step because the election is over. AEC did this in 2019. Doesn't affect who is elected, only reports.
      | --doMarginOptimization     try to compute margins (very slow)
      | --modify file              specify a file of modifications to the votes
      | --ocrErr spec              run with simulated ocr errors. spec can be
      |         Truncate:pRange    for all positions, truncate at position with probability p.
      |         DigitError:pRange  for all digits, replaced with random digit (possibly self) with probability p
      |         DigitTable:p00,p01,p02...p09,p10,p11...p99 replace digit i with j with probability pij
      |           pRange may be a single probability, or a range like 0-1:0.001 meaning numbers between 0 and 1 by 0.001.
      | --numRuns n                specify number of times to rerun with simulated ocr modification errors. (default 1)
      | --numThreads n             specify number of threads to use when running multiple times. (default 1)
      | --probFile <file>          write out a csv file containing a summary of the number of times candidates got elected in ocrErr scenarios.
      | --ocrMinFormalBTL n        The minimum number of unique consecutive preferences starting from 1 for a formal BTL vote (default rules dependent, 6 for federal)
      | --ocrMinFormalATL n        The minimum number of unique consecutive preferences starting from 1 for a formal ATL vote (default rules dependent, 1 for federal)
      | --ocrExampleDir            a directory into which to write a full distribution of preferences for the first ocr error example, will be appended by prob
      | --ocrStatsDir              a directory into which to write statistics for the ocr simulation, will be appended by prob if not too long
      |  """.stripMargin) else try {
    var rules = "federal"
    var overrideMinFormalBTL : Option[Int] = None
    var overrideMinFormalATL : Option[Int] = None
    var stvFile : Option[File] = None
    var modifyFile : Option[File] = None
    var outDir : Option[File] = None
    var ocrExampleOutDir : Option[File] = None
    var probFile : Option[File] = None
    var ocrStatsDir : Option[File] = None
    var numSeats = 6
    var processedArgs = 0
    var numRuns = 1
    var numThreads = 1
    var ecOrder : List[Int] = Nil
    var exclude : Set[Int] = Set.empty
    var prohibitMultipleEliminations = false
    var finishExclusionEvenIfAllWillBeElected = false
    var finishSuplusDistributionEvenIfEveryoneWillGetElected = false
    var interruptExclusionAtStartOfExclusionIfAllWillBeElected = false
    var doMarginOptimization = false
    var ocrError : Seq[OCRError] = Nil
    def nextArg() : String = {
      if (processedArgs==args.length) throw new IllegalArgException("Missing final argument")
      val res = args(processedArgs)
      processedArgs+=1
      res
    }
    def nextArgInt() : Int = try { nextArg().toInt } catch { case e:NumberFormatException => throw new IllegalArgException("Expecting integer argument, got "+args(processedArgs-1))}
    def nextArgInts() : Array[Int] = try { nextArg().split(',').map{_.toInt} } catch { case e:NumberFormatException => throw new IllegalArgException("Expecting integer argument list, got "+args(processedArgs-1))}
    def nextArgFile(shouldExist:Boolean=true) : File = {
      val f = new File(nextArg())
      if (shouldExist && !f.isFile) throw new IllegalArgException("Expecting "+f+" to be a file")
      f
    }
    while (processedArgs<args.length) {
      nextArg() match {
        case "--stv" => stvFile=Some(nextArgFile())
        case "--out" => outDir=Some(nextArgFile(false))
        case "--ocrExampleDir" => ocrExampleOutDir=Some(nextArgFile(false))
        case "--ocrStatsDir" => ocrStatsDir=Some(nextArgFile(false))
        case "--probFile" => probFile=Some(nextArgFile(false))
        case "--modify" => modifyFile=Some(nextArgFile())
        case "--rules" => rules=nextArg()
        case "--NumSeats" => numSeats=nextArgInt()
        case "--ECOrder" => ecOrder = nextArgInts().toList
        case "--exclude" => exclude = nextArgInts().toSet
        case "--prohibitMultipleEliminations" => prohibitMultipleEliminations=true
        case "--finishExclusionEvenIfAllWillBeElected" => finishExclusionEvenIfAllWillBeElected=true
        case "--finishSuplusDistributionEvenIfEveryoneWillGetElected" => finishSuplusDistributionEvenIfEveryoneWillGetElected=true
        case "--interruptExclusionAtStartOfExclusionIfAllWillBeElected" => interruptExclusionAtStartOfExclusionIfAllWillBeElected=true
        case "--doMarginOptimization" => doMarginOptimization=true
        case "--ocrErr" => ocrError = OCRError(nextArg())
        case "--numRuns" => numRuns=nextArgInt()
        case "--numThreads" => numThreads=nextArgInt()
        case "--ocrMinFormalBTL" => overrideMinFormalBTL=Some(nextArgInt())
        case "--ocrMinFormalATL" => overrideMinFormalATL=Some(nextArgInt())
        case unknown => throw new IllegalArgException("Unknown argument "+unknown)
      }
    }
    if (stvFile.isEmpty) throw new IllegalArgException("Need to specify vote data file.")
    val cRules : ElectionCountRules = rules match {
      case "federal" => FederalSenate2016Rules
      case "Federal2016" => FederalSenate2016Rules
      case "Federal2013" => FederalSenate2013Rules
      case "vic" => VictoriaSenate2014Rules
      case "Victoria2014" => VictoriaSenate2014Rules
    }

    def count(data:ElectionData): List[CandidateIndex] = {
      val winners = rules match { // TODO make running depend on ElectionCountRules rather than a switch here and later on as well.
        case "federal" => FederalSenateCount.run(data, numSeats, ecOrder, Map.empty,None,exclude,outDir.map{new ReportSaverDirectory(_)},prohibitMultipleEliminations,finishExclusionEvenIfAllWillBeElected,finishSuplusDistributionEvenIfEveryoneWillGetElected,doMarginOptimization,interruptExclusionAtStartOfExclusionIfAllWillBeElected)
        case _ => throw new IllegalArgException("Don't understand rules : "+rules)
      }
      for (w<-winners) println("Winner : "+data.meta.candidates(w).name)
      winners
    }
    val data = ElectionDataFastIO.loadPickled(stvFile.get)
    val normalWinners = count(data)
    for (tampering<-modifyFile) {
      val newWinners = count(data.tamperMichelleFormat(tampering))
      val change = ElectionChanged(normalWinners,newWinners)
      println("Change : "+change.descListWithGroups(data.meta))
    }
    val multistats = new MultipleElectionStats("ocr")
    for (ocr<-ocrError) {
      val makeDataLock = new Object
      val internalStats : Option[OCRStats] = for (statsDir<-ocrStatsDir) yield new OCRStats(if (ocr.parameter.length<100) new File(statsDir.getParent,statsDir.getName+" "+ocr.parameter) else statsDir,numRuns,ocr.parameter)
      println("\n\nRunning error rate "+ocr.parameter+"\n")
      val minFormalATL = overrideMinFormalATL.getOrElse(cRules.minATLmarksToBeValid)
      val minFormalBTL = overrideMinFormalBTL.getOrElse(cRules.minBTLmarksToBeValid)
      val runner = new ProbabilisticRunner(None) {
        /** Do the actual probabilistic run, possibly concurrently with other runs */
        override def doOneRun(random: Random): ElectionResultReport = {
          def makeData() = data.simulateOCRerror(random,ocr,minFormalATL,minFormalBTL,internalStats)
          val newdata = internalStats match {
            case Some(s) =>
              makeDataLock.synchronized{// need to present the stats data in a serialised way.
                val res =  makeData()
                s.endElection()
                res
              }
            case None => makeData() // no need to do in a thread serialized manner.
          }
          //ElectionDataFastIO.savePickled(newdata,new File("test.stl"))
          //newdata.printStatus()
          rules match {
            case "federal" =>
              val worker = new FederalSenateCountHelper(newdata,numSeats,Map.empty,ecOrder,false,exclude,prohibitMultipleEliminations,finishExclusionEvenIfAllWillBeElected,finishSuplusDistributionEvenIfEveryoneWillGetElected,interruptExclusionAtStartOfExclusionIfAllWillBeElected)
              worker.run(None)
              //ElectionReport.saveReports(new File("OCR"),worker.report,newdata.meta)
              worker.report
          }
        }
      }
      val stats = runner.runProbabilisticly(data.meta,numRuns,numThreads,true,None,ocrExampleOutDir.map{f=>new File(f.getParent,f.getName+ocr.parameter)})
      multistats.add(ocr.parameter,stats)
      for (s<-internalStats) s.close()
    }
    for (f<-probFile) multistats.printTable(f)
  } catch {
    case e:IllegalArgException => println("Error in arguments : "+e.getMessage)
  }

}

class IllegalArgException(message:String) extends Exception(message)



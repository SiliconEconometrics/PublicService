/*
    Copyright 2016-2019 Silicon Econometrics Pty. Ltd.

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

import org.greatcactus.vote.count.ballots.{ElectionData, ElectionDataFastIO}
import org.greatcactus.vote.count.federal.FederalSenateCount
import org.greatcactus.vote.count.margin.ElectionChanged

object MainApp extends App {
  if (args.length==0) println(
    """
      | Allowable arguments
      | --stv <file>               specify file of election information, in Andrew stv format. You can get format and data from vote.andrewconway.org
      | --rules rulename           specify rules used for running the election. Default "federal".
      | --NumSeats number          specify number of candidates to elect (default 6).
      | --ECOrder candidatelist    specify a comma separated list of candidate numbers that define the order the EC breaks ties.
      | --exclude candidatelist    specify a comma separated list of candidate numbers to be deemed ineligible
      | --prohibitMultipleEliminations                           For federal elections, don't use section 13(A) multiple eliminations. AEC didn't use this in 2016 and 2019.
      | --finishExclusionEvenIfAllWillBeElected                  Don't stop an exclusion just because the election is over. AEC did this in 2016 and 2019. Doesn't affect who is elected, only reports.
      | --finishSuplusDistributionEvenIfEveryoneWillGetElected   Don't stop an surplus distribution just because the election is over. AEC did this in 2016 and 2019. Doesn't affect who is elected, only reports
      | --doMarginOptimization     try to compute margins (very slow)
      | --modify file              specify a file of modifications to the votes
      |      |    """.stripMargin) else try {
    var rules = "federal"
    var stvFile : Option[File] = None
    var modifyFile : Option[File] = None
    var numSeats = 6
    var processedArgs = 0
    var ecOrder : List[Int] = Nil
    var exclude : Set[Int] = Set.empty
    var prohibitMultipleEliminations = false
    var finishExclusionEvenIfAllWillBeElected = false
    var finishSuplusDistributionEvenIfEveryoneWillGetElected = false
    var doMarginOptimization = false
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
        case "--modify" => modifyFile=Some(nextArgFile())
        case "--rules" => rules=nextArg()
        case "--NumSeats" => numSeats=nextArgInt()
        case "--ECOrder" => ecOrder = nextArgInts().toList
        case "--exclude" => exclude = nextArgInts().toSet
        case "--prohibitMultipleEliminations" => prohibitMultipleEliminations=true
        case "--finishExclusionEvenIfAllWillBeElected" => finishExclusionEvenIfAllWillBeElected=true
        case "--finishSuplusDistributionEvenIfEveryoneWillGetElected" => finishSuplusDistributionEvenIfEveryoneWillGetElected=true
        case "--doMarginOptimization" => doMarginOptimization=true
        case unknown => throw new IllegalArgException("Unknown argument "+unknown)
      }
    }
    if (stvFile.isEmpty) throw new IllegalArgException("Need to specify vote data file.")
    def count(data:ElectionData) = {
      val winners = rules match {
        case "federal" => FederalSenateCount.run(data, numSeats, ecOrder, Map.empty,None,exclude,None,prohibitMultipleEliminations,finishExclusionEvenIfAllWillBeElected,finishSuplusDistributionEvenIfEveryoneWillGetElected,doMarginOptimization)
        case _ => throw new IllegalArgException("Don't understand rules : "+rules)
      }
      for (w<-winners) println("Winner : "+data.meta.candidates(w).name)
      winners
    }
    val data = ElectionDataFastIO.loadPickled(stvFile.get)
    val normalWinners = count(data)
    for (tampering<-modifyFile) {
      val newWinners = count(data.tamperMichelleFormat(tampering))
      val change = new ElectionChanged(normalWinners,newWinners)
      println("Change : "+change.descListWithGroups(data.meta))
    }
    // TODO need to do something with manipulations

  } catch {
    case e:IllegalArgException => println("Error in arguments : "+e.getMessage)
  }

}

class IllegalArgException(message:String) extends Exception(message)



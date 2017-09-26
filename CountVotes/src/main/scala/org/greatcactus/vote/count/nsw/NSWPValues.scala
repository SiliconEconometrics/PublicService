/*
    Copyright 2016-2017 Silicon Econometrics Pty. Ltd.

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

    Analyze probability of NSW outcomes
 */

package org.greatcactus.vote.count.nsw
import org.greatcactus.vote.count._

import org.jsoup.nodes.Document

case class CountActionDistributeExcess(val who:Int,val tvNumerator:Int,val tvDenominator:Int)

/** A single count from NSW offical DoP */
class NSWCountSummary(
    val tallyAtStart:Array[Int],
    val distributed:Array[Int],
    val transferrred:Array[Int],
    /* Eliminated=-1; Elected = 10^9, otherwise votes */ val tallyAtEnd:Array[Int],
    val exhaustedAtEnd:Int
) {
  val isFirstCount : Boolean = tallyAtStart.max==0
  val excludedCandidate : Option[Int] = (0 until tallyAtStart.length).find{i=>tallyAtStart(i)>0 && tallyAtEnd(i)<=0}
  val distributedExcess : Option[CountActionDistributeExcess] = (0 until tallyAtStart.length).find{i=>tallyAtStart(i)>0 && tallyAtEnd(i)>0 && tallyAtStart(i)>tallyAtEnd(i)}.map{who=>new CountActionDistributeExcess(who,transferrred.sum,distributed.sum)}
  def printout() {
    println("Start\t"+tallyAtStart.mkString("\t"))
    println("Distrib\t"+distributed.mkString("\t"))
    println("xter\t"+transferrred.mkString("\t"))
    println("End\t"+tallyAtEnd.mkString("\t"))
  }
}
    
class NSWWholeCountSummary(val counts:Array[NSWCountSummary]) {
  def printout() {
    for (i<-0 until counts.length) { 
      println("Count "+(i+1))
      counts(i).printout()
    }
  }
}

class NumDistribution {
  var seen : Map[Int,Int] = Map.empty // how many times a particular integer was seen
  /** Add an occurrence of i */
  def add(i:Int) { seen+=i->(1+(seen.getOrElse(i,0))); }
  /** See how many values are at least this extreme */
  def extremeness(i:Int) : Int = {
    var above = 0
    var below = 0
    for ((key,value)<-seen) {
      if (key<=i) below+=value
      if (key>=i) above+=value
    }
    above min below
  }
  def extremenessDesc(i:Int) : String = {
    var above = 0
    var below = 0
    var equal = 0
    for ((key,value)<-seen) {
      if (key<i) below+=value
      if (key>i) above+=value
      if (key==i) equal+=value
    }
    i.toString+" has "+below+" below, "+equal+" equal and "+above+" above."
  }
  /** Proportion <=i */
  def lessOrEqual(i:Int) : Double = {
    var above = 0
    var below = 0
    for ((key,value)<-seen) {
      if (key<=i) below+=value
      if (key>i) above+=value // note different to extremeness() function
    }
    below/(above+below).toDouble
  }
}

class CountNumDistribution(numCandidates:Int) {
  var total : Int = 0
  val candidates : Array[NumDistribution] = Array.fill(numCandidates)(new NumDistribution)
  val exhausted = new NumDistribution
  
  def add(sum:NSWCountSummary) {
     for (i<-0 until numCandidates) candidates(i).add(sum.tallyAtEnd(i))
     exhausted.add(sum.exhaustedAtEnd)
     total+=1
  }
  def extremeness(sum:NSWCountSummary) : Int = {
    var res = exhausted.extremeness(sum.exhaustedAtEnd)
    for (i<-0 until numCandidates) res=res min candidates(i).extremeness(sum.tallyAtEnd(i))
    res
  } 
  def printoutDetailedExtremeness(sum:NSWCountSummary,candidateNames:Array[String]) {
    println("   Exhausted : "+sum.exhaustedAtEnd+" => "+exhausted.extremeness(sum.exhaustedAtEnd))
    for (i<-0 until numCandidates) println("  "+candidateNames(i)+" : "+sum.tallyAtEnd(i)+" => "+candidates(i).extremeness(sum.tallyAtEnd(i)))
  }

}
class NSWAllCountsSummary(val numCandidates:Int) {
  val maxRounds = numCandidates*2+1 // extrememely conservative
  var maxrounds = -1
  var total = 0
  val counts = Array.fill(maxRounds){new CountNumDistribution(numCandidates)}
  def add(sum:NSWWholeCountSummary) {
    for (i<-0 until sum.counts.length) counts(i).add(sum.counts(i))
    total+=1
    maxrounds = maxrounds max sum.counts.length
  }
  def extremeness(sum:NSWWholeCountSummary) : Int = {
    var res = total
    for (i<-0 to maxrounds) res=res min {
      if (i<sum.counts.length) counts(i).extremeness(sum.counts(i))
      else total-counts(i).total
    }
    res
  }
  def printoutDetailedExtremeness(sum:NSWWholeCountSummary,candidateNames:Array[String]) {
    for (i<-0 until sum.counts.length) {
      println(" Count "+(i+1)+"  => "+counts(i).extremeness(sum.counts(i)))
      counts(i).printoutDetailedExtremeness(sum.counts(i),candidateNames:Array[String])
    }
    
  }

  def extremenessReport(sum:NSWWholeCountSummary,candidateNames:Array[String],name:String,score:Int,pValue:Double) = {
    <html>
      <head>
        <meta charset="UTF-8"/> 
        <title>Official vs. distribution for {name}</title>
        <link href="report.css" type="text/css" rel="stylesheet"></link>
      </head>
      <body>
        <p>Overall score {score} of {total} P-Value {pValue}</p>
        <table class="Display">
          <tr class="Head"><th>Count</th>{for (i<-0 until sum.counts.length) yield <th>{i+1}</th>}</tr>
          {
            for (c<-0 until candidateNames.length) yield <tr class="Striped"><td>{candidateNames(c)}</td>{for (i<-0 until sum.counts.length) yield <td title={counts(i).candidates(c).extremenessDesc(sum.counts(i).tallyAtEnd(c))}>{counts(i).candidates(c).extremeness(sum.counts(i).tallyAtEnd(c))}</td>}</tr>
          }
          <tr class="Exhausted"><td>Exhausted</td>{for (i<-0 until sum.counts.length) yield <td title={counts(i).exhausted.extremenessDesc(sum.counts(i).exhaustedAtEnd)}>{counts(i).exhausted.extremeness(sum.counts(i).exhaustedAtEnd)}</td>}</tr>
          <tr class="Exhausted"><td>Min</td>{for (i<-0 until sum.counts.length) yield <td>{counts(i).extremeness(sum.counts(i))}</td>}</tr>
        </table>
        <p>Finishing that early : {(total-counts(sum.counts.length).total)}</p>
      </body>
    </html>
  }
  
}

class NSWDistributionSummary(val numCandidates:Int) {
  val extremenessSummary = new NSWAllCountsSummary(numCandidates)
  val extremenessDistribution = new NumDistribution
  val components = new collection.mutable.ArrayBuffer[NSWWholeCountSummary]
  
  /** Biased but efficient method - use same set for generating and evaluating function. Probably a bad idea to use */
  def addBiased(sum:NSWWholeCountSummary) {
    components+=sum
    extremenessSummary.add(sum)
  }
  /** Use a run to generate a function describing extremeness */
  def addJustGenerateFunction(sum:NSWWholeCountSummary) { extremenessSummary.add(sum) }
  /** Use a run to evaluate the distribution of said function */
  def addJustEvaluateFunctionDistribution(sum:NSWWholeCountSummary) { extremenessDistribution.add(extremenessSummary.extremeness(sum)) }
  private def makeDistribution() {
    for (c<-components) extremenessDistribution.add(extremenessSummary.extremeness(c))
    components.clear()
  }
  /** Calculate the extremeness, and a P value for it */
  def extremeness(sum:NSWWholeCountSummary) : (Int,Double) = {
    /** See if extremenessDistribution needs making */
    if (!components.isEmpty) makeDistribution()
    val extremeness = extremenessSummary.extremeness(sum)
    (extremeness,extremenessDistribution.lessOrEqual(extremeness))
  }
  
  def printoutDetailedExtremeness(sum:NSWWholeCountSummary,candidateNames:Array[String]) {
    /** See if extremenessDistribution needs making */
    if (!components.isEmpty) makeDistribution()
    extremenessSummary.printoutDetailedExtremeness(sum,candidateNames)
  }
  def extremenessReport(sum:NSWWholeCountSummary,candidateNames:Array[String],name:String) ={
    val (score,p) = extremeness(sum)
    extremenessSummary.extremenessReport(sum,candidateNames,name,score,p)
  }
}



object NSWWholeCountSummary {
  def ofReport(report:ElectionResultReport) : NSWWholeCountSummary = {
    val res = new NSWWholeCountSummary(report.history.toArray.map{NSWCountSummary.ofReport(_)})
    val no1exhaust = new NSWWholeCountSummary(res.counts.map{c=>new NSWCountSummary(c.tallyAtStart,c.distributed,c.transferrred,c.tallyAtEnd,c.exhaustedAtEnd-res.counts(0).exhaustedAtEnd)}) // compensate for 2016 NSW LGE not including exhausted in first count votes in subsequent counts.
    no1exhaust
  }
}
object NSWCountSummary {
  val excludedSpecialCode = -1
  val ineligibleSpecialCode = -2
  val electedSpecialCode = 1000000000
  
  def ofReport(step:ElectionCountReport) : NSWCountSummary = {
    val counts = if (step.end!=null) step.end else step.start
    val tallys = counts.votesPerCandidate.map{_.toInt}
//    for (i<-counts.electedCandidates) tallys(i)=electedSpecialCode
    for (i<-counts.excludedCandidates) tallys(i)=excludedSpecialCode
    for (i<-counts.ineligibleCandidates) tallys(i)=ineligibleSpecialCode    
    val tallyAtStart:Array[Int] = if (step.start!=null) step.start.votesPerCandidate.map{_.toInt} else new Array[Int](tallys.length)
    val distributed:Array[Int] = step.ballotPapersDistributed.map{_.toInt}
    val transferrred:Array[Int] = Array.tabulate(tallys.length){i=>step.ballotPapersTransferred(i).toInt}
    new NSWCountSummary(tallyAtStart,distributed,transferrred,tallys,counts.exhausedVotes.toInt)
  }
  
  
  import scala.collection.JavaConverters._
  def ofJSoup(doc:Document,candidateNames:Array[String]) : NSWCountSummary = {
    val tally = new collection.mutable.ArrayBuffer[Int]
    val startCounts = new Array[Int](candidateNames.length)
    val transCounts = new Array[Int](candidateNames.length)
    val distCounts = new Array[Int](candidateNames.length)
    var exhausted:Int=0
    val desc = {
      doc.select("h2").first().text.trim+" : "
    }
    for (table<-doc.select("table").asScala) {
      val rows = table.select("tr").asScala
      val headings : Map[String,Int] = Map(rows.head.select("th").asScala.map{_.text.trim}.zipWithIndex :_*)
      headings.get("Candidates in Ballot Order") match {
        case None =>
        case Some(candidateCol) =>
         val groupCol = headings.get("Group")
         var groupCount = 0
         val totalCol = headings.get("Progressive Total").orElse(headings.get("Votes")).orElse(headings.get("First Preferences")).getOrElse(throw new Exception("Document has "+headings+" but no total."))
         val transCol = headings.get("Ballot Papers Transferred")
         val startCol = headings.get("Progressive Total Brought Forward")
         val distributedCol = headings.get("Ballot Papers Distributed")
         def parseNum(num:String) : Int = {
           if (num=="EXCLUDED") excludedSpecialCode
           else if (num=="ELECTED") electedSpecialCode
           else if (num=="INELIGIBLE") ineligibleSpecialCode
           else if (num=="") 0
           else num.replace(",","").toInt
         }
         for (row<-rows.tail) {
           val entries : IndexedSeq[String] = row.select("td").asScala.toIndexedSeq.map{_.text.replace('\u00a0',' ').trim}
           def optNum(col:Option[Int]) : Int = if (col.isDefined) { val n = parseNum(entries(col.get)); if (n==electedSpecialCode||n==excludedSpecialCode||n==ineligibleSpecialCode) 0 else n} else 0
           val numTotal = parseNum(entries(totalCol))+groupCount
           groupCount=0
           val numStart = optNum(startCol)
           val numTrans = optNum(transCol)
           val numDist  = optNum(distributedCol)
           val name = entries(candidateCol)
           val groupStart = groupCol.map{entries(_)}.getOrElse("")
           if (groupStart.length>0) { groupCount = numTotal } // group should be carried on to next.
           else if (name=="UNGROUPED CANDIDATES") {  }
           else if (name=="Formal Votes") {}
           else if (name=="Informal Ballot Papers") {}
           else if (name=="Total Votes / Ballot Papers") {}
           else if (name=="Group Total") {}
           else if (name=="TOTAL") {}
           else if (name=="Brought Forward") {exhausted+=numTotal}
           else if (name=="Set Aside this Count") {exhausted+=numTotal}
           else if (name=="Set Aside (previous counts)") {}
           else if (name=="Exhausted") { /* exhausted+=num */} // this value is not actually maintained in subsequent counts.
           else {
             if (candidateNames.length<=tally.length || candidateNames(tally.length)!=name) throw new Exception(desc+"Was not expecting "+name+" starting code "+name.charAt(0).toInt+" was expecting "+(if (candidateNames.length<=tally.length) "an end" else candidateNames(tally.length)+" in row containing "+entries))
             val isElected = transCol.isDefined && entries(transCol.get)=="ELECTED"
             startCounts(tally.length)=numStart
             transCounts(tally.length)=numTrans
             distCounts(tally.length)=numDist
             tally+= numTotal // (if (isElected) electedSpecialCode else num)
           }
         }
      }
    }
    if (tally.length!=candidateNames.length) throw new Exception(desc+"Tally.length "+tally.length+" != candidateNames.length "+candidateNames.length+" "+candidateNames.mkString(";"))
    new NSWCountSummary(startCounts,distCounts,transCounts,tally.toArray,exhausted)
  }
}

/** Check how the NSWEC handles ties in eliminations. Done because of (IMHO) bugs in their 2016 counts for campbelltown and hawkesbury  */
object CheckTies {
  /** Given a set of tied people, find the smallest one(s) in the given count final tallys */ 
  private def resolveTiesForSmallestCountback(ties:Seq[Int],sum:NSWCountSummary) : Seq[Int] = {
    val min = ties.map { sum.tallyAtEnd(_) }.min
    ties.filter{ sum.tallyAtEnd(_)==min}
  }
  private def resolveTiesForSmallestCountback(ties:Seq[Int],summary:NSWWholeCountSummary,countNum:Int) : Seq[Int] = {
    summary.counts.take(countNum-1).reverse.foldLeft(ties){(t,c)=>resolveTiesForSmallestCountback(t,c)}
  }
  def printBackTrace(summary:NSWWholeCountSummary,candidateNames:Array[String],biggestCountToShow:Int,ties:Seq[Int]) {
            println("Count\t"+ties.map{candidateNames(_)}.mkString("\t"))
            for (cNum<- biggestCountToShow to 0 by -1) {
              val tallys = summary.counts(cNum).tallyAtEnd
              println((cNum+1).toString+"\t"+ties.map{tallys(_).toString}.mkString("\t"))
            }            
  }

  def check(summary:NSWWholeCountSummary,candidateNames:Array[String]) {
    // summary.printout()
    val numCandidates = candidateNames.length
    for (countNum<-0 until summary.counts.length) {
      val count =summary.counts(countNum)
      for (personExcluded<-count.excludedCandidate) {
        //println("Checking exclusion count "+(countNum+1))
        val excludedTally = count.tallyAtStart(personExcluded)
        val lower = (0 until numCandidates).filter{i=>count.tallyAtStart(i)>0 && count.tallyAtStart(i)<excludedTally}
        for (who<-lower) println("**** ERROR "+candidateNames(who)+" has a lower tally than "+candidateNames(personExcluded)+" at count "+(countNum+1))
        val ties : Seq[Int] = (0 until numCandidates).filter{i=>count.tallyAtStart(i)>0 && count.tallyAtStart(i)==excludedTally}
        if (ties.length>2) {
          println(ties.length.toString+" way tie at count "+(countNum+1)+" on "+excludedTally+" votes. "+candidateNames(personExcluded)+" was excluded.")
          val afterCountback = resolveTiesForSmallestCountback(ties,summary,countNum)
          if (afterCountback.length>1) {
            println("Countback leave a random draw amongst "+afterCountback.length)
          }
          def printBacktrace() { printBackTrace(summary,candidateNames,countNum-1,ties) }
/*
          def printBacktrace() {
            println("Count\t"+ties.map{candidateNames(_)}.mkString("\t"))
            for (cNum<- (countNum-1) to 0 by -1) {
              val tallys = summary.counts(cNum).tallyAtEnd
              println((cNum+1).toString+"\t"+ties.map{tallys(_).toString}.mkString("\t"))
            }            
          }*/
          var wantBacktrace = false
          if (afterCountback.length==1) {
            val others = resolveTiesForSmallestCountback(ties.filter{!afterCountback.contains(_)},summary,countNum)
            if (others.length>1) { wantBacktrace=true; println("While there is a clear loser "+candidateNames(afterCountback(0))+", there is not a clear second. Not that it is needed.") }
          }
          if (!afterCountback.contains(personExcluded)) {
            println("**** ERROR! "+candidateNames(personExcluded)+" is ahead of "+afterCountback.map{candidateNames(_)}.mkString(" and ")+" on the countback.")
            wantBacktrace=true
          }
          if (wantBacktrace) printBacktrace()
        }
      }
    }
    
  }
}

/** Check how the NSWEC handles rounding. Done because of a bug in the 2016 NSWEC counts for Bland Shire Council */ 
object CheckRounding {
  val roundFractionPartBySelf = true // should be true, get numerical errors
  val decimalPlacesToRoundTo = 6
  /** Round x to a specified number of decimal places */
  def round(x:Double,decimalPlaces:Int) : Double = {
    var mul = 1.0;
    for (_ <- 0 until decimalPlaces) mul*=10
    Math.round(mul*x)/mul
  }
  def check(summary:NSWWholeCountSummary,candidateNames:Array[String]) {
    val numCandidates = candidateNames.length
    for (countNum<-0 until summary.counts.length) {
      val count =summary.counts(countNum)
      for (excessInfo<-count.distributedExcess) {
        def printBacktrace(ties:Seq[Int]) { CheckTies.printBackTrace(summary,candidateNames,countNum-1,ties) }
        val tv = round(excessInfo.tvNumerator.toDouble/excessInfo.tvDenominator,16)
        // println("Checking distribution count "+(countNum+1)+" tv="+tv)
        val deserve = Array.tabulate(numCandidates){i=>tv*count.distributed(i)}
        val deserveInt = deserve.map{_.toInt}
        val frac = deserve.map{x=>if (roundFractionPartBySelf) round(x-x.toInt,decimalPlacesToRoundTo) else (round(x,decimalPlacesToRoundTo)-x.toInt) }
        val numToRoundUp = excessInfo.tvNumerator-deserveInt.sum
        val (whoToRoundUp:Set[Int],extraRandomPeopleToRoundUp:Int,randomPeopleToRoundUp:Set[Int]) = 
         if (numToRoundUp==0) (Set.empty,0,Set.empty)
         else if (numToRoundUp==numCandidates) ((0 until numCandidates).toSet,0,Set.empty)
         else  {
          val sortedCandidatesByFractionalPart = (0 until numCandidates).toArray.sortBy{-deserveInt(_)}.sortBy{-frac(_)}
          val criticalFrac = frac(sortedCandidatesByFractionalPart(numToRoundUp-1))
          val (definites:List[Int],numRandom:Int,randoms:List[Int]) = if (criticalFrac == frac(sortedCandidatesByFractionalPart(numToRoundUp))) {
            val whoHasSameFrac = sortedCandidatesByFractionalPart.filter{frac(_)==criticalFrac}
            val numToRoundUpWithSameFraction = sortedCandidatesByFractionalPart.take(numToRoundUp).filter{frac(_)==criticalFrac}.length
            println("Need to choose "+numToRoundUpWithSameFraction+" of "+whoHasSameFrac.length+" with equal fractions for count "+(countNum+1)+". Critical fraction = "+criticalFrac+" posessed by "+(whoHasSameFrac.map{c=>candidateNames(c)+" "+deserveInt(c)}).mkString(" and "))
            val criticalInt = deserveInt(sortedCandidatesByFractionalPart(numToRoundUp-1))
            if (criticalInt == deserveInt(sortedCandidatesByFractionalPart(numToRoundUp))) { // need to do by countback.
              val whoHasSameInt = whoHasSameFrac.filter{deserveInt(_)==criticalInt}
              val numToRoundUpWithSameInt = sortedCandidatesByFractionalPart.take(numToRoundUp).filter{frac(_)==criticalFrac}.filter{deserveInt(_)==criticalInt}.length
              println("Need to choose "+numToRoundUpWithSameInt+" of "+whoHasSameInt.length+" with equal fractions and ints for count "+(countNum+1)+". Critical int = "+criticalInt+", critical fraction = "+criticalFrac+" posessed by "+(whoHasSameInt.map{c=>candidateNames(c)}).mkString(" and "))
              (List.empty,numToRoundUpWithSameInt,whoHasSameInt.toList)
            } else {
              println("--- Resolved by integers")
              //printBacktrace(whoHasSameFrac)
              (List.empty,0,List.empty)
            }
          } else (List.empty,0,List.empty)
          (sortedCandidatesByFractionalPart.take(numToRoundUp-definites.length-numRandom).toSet++definites,numRandom,randoms.toSet)
        }
        def expected(candidate:Int) : Set[Int] = {
          val base = deserveInt(candidate)+(if (whoToRoundUp.contains(candidate)) 1 else 0)
          if (randomPeopleToRoundUp(candidate)) Set(base,base+1) else Set(base)
        }
        for (candidate<-0 until numCandidates) {
          if (!expected(candidate).contains(count.transferrred(candidate))) println("*** ERROR in count "+(countNum+1)+" Candidate "+candidateNames(candidate)+" has "+count.transferrred(candidate)+" expecting "+expected(candidate).mkString(" or "))
        }
      }
    }
  }  
}
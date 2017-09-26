/*
    Copyright 2015-2017 Silicon Econometrics Pty. Ltd.

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

import org.greatcactus.vote.count.{ElectionCountReport, ElectionResultReport, FindBaseDir}
import org.greatcactus.vote.count.MainDataTypes._
import org.junit.Assert._
import org.junit.Test;

/**
  * Test my algorithm against official distribution of preferences.
  *
  * Based on my possibly flawed understanding of the datafiles, there appears to be an error in the Mining and Pastoral Region, first preference counts
  * for RENTON, Darby. The official results give 72. The XML file ...LC_VERBOSE_RESULTS.xml says there are 42 ticket votes and 30 non-ticket votes. However,
  * looking at the BTL file "CountWA Export - 11 March 2017 State General Election.Mining and Pastoral Region.(Ballot-Papers).csv", there are 34
  * btl votes with a 1 for RENTON, Darby, of which 5 are marked as informal, leaving 29 non-ticket votes, ie 71 total. This does not change who is elected,
  * but I have not otherwise checked correctness as it makes a difference at every line as the total number of votes is different. Maybe my file
  * got corrupted in transmission.
  *
  */
class TestWA2017 {

  FindBaseDir.findBaseDir()

  def test(region:WARegion,waecDeemedOrder:Seq[CandidateIndex]) {
    val data = region.data
    data.printStatus()
    val officialResults = WA2017OfficialResults.load(region)
    val worker = new WAElectionHelper(data,officialResults.vacancies,Map.empty,waecDeemedOrder,true)
    worker.run()
    val myreport = worker.report
    assertEquals("Quota",officialResults.quota,myreport.quota)
    var upToInMine = -1
    for (countNo<-officialResults.counts.indices) {
      val official = officialResults.counts(countNo)
      val myVotesDelta = new Array[Int](data.numCandidates)
      var myLostDelta = 0
      def addDeltas(m:ElectionCountReport): Unit = {
        for (i<-0 until data.numCandidates) myVotesDelta(i)+=m.ballotPapersTransferred(i).toInt
        myLostDelta+=Math.round(m.roundingTransferred).toInt
      }
      var isDisambiguatedByEC : Boolean = false
      val my = {
        upToInMine+=1
        if (myreport.history.length<=upToInMine) fail("My report is too short, only "+myreport.history.length)
        isDisambiguatedByEC = !myreport.history(upToInMine).equalCandidatesDisabiguatedByEC.isEmpty
        val desc = myreport.history(upToInMine).countType.structureDesc(region.data.candidates,false)
        while (upToInMine+1<myreport.history.length && desc==myreport.history(upToInMine+1).countType.structureDesc(region.data.candidates,false) &&myreport.history(upToInMine).electedCandidates.isEmpty) {
          addDeltas(myreport.history(upToInMine))
          upToInMine+=1
        }
//        println("desc: "+desc)
//        println("next: "+myreport.history(upToInMine+1).structureDesc(region.data.candidates,false))
        myreport.history(upToInMine)
      }
      addDeltas(my)
      val desc = "Count "+official.who+" "+official.what + " "+my.prettyCountName

      if (data.name == "Mining and Pastoral") {
        // don't check things. I disagree with the official results. See comment at start of test class.
      } else {
        // test what the count is about
        val prefix = if (isDisambiguatedByEC) "*" else ""
        if (official.what=="Exclusion") assertEquals(desc,official.who,prefix+data.candidates(my.countType.candidatesEliminated.head).name)
        else if (official.what=="Surplus") assertEquals(desc,official.who,prefix+data.candidates(my.countType.candidateDistributed).name)
        else if (official.what=="") assertEquals(desc,"First Preferences",official.who)

        // test candidates
        for (i<-0 until data.numCandidates) {
          val name = desc+" " + data.candidates(i).name
          assertEquals(name,official.votesDelta(i),myVotesDelta(i))
          if (official.votesCum!=null) assertEquals(name,official.votesCum(i),my.totalAtEnd(i).toInt)
        }
        // test rounding
        assertEquals(desc,official.lostDelta,myLostDelta,1e-7)
        assertEquals(desc,official.lostCum,my.roundingAtEnd,1e-7)
        // see if same candidates elected
        val myElected = my.electedCandidates.toArray.map{_._1}
        assertEquals(desc,official.electedCandidates.mkString(","),myElected.mkString(","))
      }
    }
    assertEquals("My report is too long",upToInMine+1,myreport.history.length)
  }

  @Test def testAll(): Unit = {
    val data = WA2017ElectionData.loadAllRaw()
    for (region<-data) test(region,WA2017OfficialResults.usedOrders.getOrElse(region.data.name,List.empty))
  }
}



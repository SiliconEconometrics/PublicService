/*
    Copyright 2015-2018 Silicon Econometrics Pty. Ltd.

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
package org.greatcactus.vote.count.federal

import org.greatcactus.vote.count._
import org.greatcactus.vote.count.federal.parsing.FederalElectionDataLoader2016
import org.junit.Assert._
import org.junit.Test

/**
 * Test my algorithm against official distribution of preferences
 */
class TestFederal2016 {

  FindBaseDir.findBaseDir()

  def test(state:String,toBeElected:Int,aecDeemedOrder:Seq[Int],excluded:Set[Int]=Set.empty) {
    val data = FederalElectionDataLoader2016.load(state)
    data.printStatus()
    val officialResults = FederalElectionDataLoader2016.readOfficialResults2016(data)
    val worker = new FederalSenateCountHelper(data,toBeElected,Map.empty,aecDeemedOrder,printDebugMessages = true,ineligibleCandidates = excluded,prohibitMultipleEliminations = true,finishExclusionEvenIfAllWillBeElected = true,finishSuplusDistributionEvenIfEveryoneWillGetElected = true,interruptExclusionAtStartOfExclusionIfAllWillBeElected = false) // compare without multiple exclusions
    worker.run(None)
    val myreport = worker.report

    for (countNo<-officialResults.counts.indices) {
      val desc = "Count "+(countNo+1)
      val official = officialResults.counts(countNo)
      if (myreport.history.length<=countNo) fail("My report is too short, only "+myreport.history.length+" expecting "+officialResults.counts.length)
      val my = myreport.history(countNo)
      //val totalVotesEnd = (0 until data.numCandidates).map{official.candidates(_).progressiveTotalVotes}.sum + official.exhausted.progressiveTotalVotes+official.rounding.progressiveTotalVotes
      //println("Official sum of votes for "+desc+" is "+totalVotesEnd)
      //val myVotesEnd = (0 until data.numCandidates).map{my.totalAtEnd}.sum + my.end.exhausedVotes+my.roundingAtEnd
      //println("      my sum of votes for "+desc+" is "+myVotesEnd)
      // test candidates
      for (i<-0 until data.numCandidates) if (!excluded.contains(i)) {
        val name = desc+" " + data.candidates(i).name
        val off = official.candidates(i)
        assertNotNull(name,off)
        assertEquals(name,off.papers,my.papersTransferred(i))
        assertEquals(name,off.votesTrans,my.ballotPapersTransferred(i).toInt)
        assertEquals(name,off.progressiveTotalVotes,my.totalAtEnd(i).toInt)
      }
      // test exhausted
      assertEquals(desc,official.exhausted.papers,my.exhaustedPapersTransferred.toInt)
      assertEquals(desc,official.exhausted.votesTrans,(my.end.exhausedVotes-(if (my.start==null) 0 else my.start.exhausedVotes)).toInt)
      assertEquals(desc,official.exhausted.progressiveTotalVotes,my.end.exhausedVotes.toInt)
      // test rounding
      assertEquals(desc,official.rounding.papers,0)
      assertEquals(desc,official.rounding.votesTrans,my.roundingTransferred,1e-7)
      assertEquals(desc,official.rounding.progressiveTotalVotes,my.roundingAtEnd,1e-7)
    }
    assertEquals("My report is too long",officialResults.counts.length,myreport.history.length)
  }
  
	@Test def testNT() { test("NT",2,DeducedAEC2016Orders.nt) }
	@Test def testVIC() { test("VIC",12,DeducedAEC2016Orders.vic) }
  @Test def testNSW() { test("NSW",12,DeducedAEC2016Orders.nsw) }
  @Test def testACT() { test("ACT",2,DeducedAEC2016Orders.act) }
  @Test def testTAS() { test("TAS",12,DeducedAEC2016Orders.tas) }
  @Test def testSA() { test("SA",12,DeducedAEC2016Orders.sa,Set(38)) } // Bob Day is excluded
  @Test def testWA() { test("WA",12,DeducedAEC2016Orders.wa,Set(45)) } // Robert Cullerton is excluded
  @Test def testQLD() { test("QLD",12,DeducedAEC2016Orders.qld) }
  
}

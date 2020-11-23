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
package org.greatcactus.vote.count.nsw
import org.greatcactus.vote.count.MainDataTypes.PaperCountUnscaled
import org.greatcactus.vote.count._
import org.greatcactus.vote.count.ballots.{ElectionData, GroupInformation}

object IVoteScreenStats extends App {
    val ivote = NSWStateElectionData2019.load(true,false)
    val normal = NSWStateElectionData2019.load(false,true)

    println("iVote")
    analyse(ivote)
    println("Normal")
    analyse(normal)
    
    def analyse(stats:ElectionData) {
      val groupIndexFromID = stats.groupIndexFromID+(""->stats.groupInfo.length)
      val groupCounter = new Array[PaperCountUnscaled](stats.groupInfo.length+1)
      for (v<-stats.satls) groupCounter(stats.groupIndexFromID(v.group))+=v.numVoters
      for (v<-stats.ratls) groupCounter(v.groups.map(stats.groupIndexFromID).max)+=v.numVoters
      for (v<-stats.btls) groupCounter(groupIndexFromID(stats.candidates(v.candidates.max).group))+=v.numVoters
      stats.printStatus()
      val total = stats.totalFormalVotes
      var cumulative : PaperCountUnscaled = 0
      for (i<-0 until stats.groupInfo.length+1) {
        val g = if (i==stats.groupInfo.length) new GroupInformation("UG","UG",Some("UG"),Array()) else stats.groupInfo(i)
        cumulative+=groupCounter(i)
        println(""+g.groupId+"\t"+groupCounter(i)+"\t"+cumulative+"\t"+"%6.2f".format(cumulative*100.0/total)+"\t"+g.groupName )
      }
    }
}

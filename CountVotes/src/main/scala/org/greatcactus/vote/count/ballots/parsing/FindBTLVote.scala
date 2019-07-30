/*
    Copyright 2019 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count.ballots.parsing

// Test at http://192.168.2.20:8095/tools/federal/2016/TAS/PrepareVote.html
// with (the first listed vote from AEC) 9,10,21,22,23,24,25,26,54,55,56,7,8,19,20,1,2,3,4,5,6,51,52,53,49,50,47,48,57,58,29,30,13,14,44,45,46,33,34,42,43,11,12,31,32,27,28,15,16,17,18,35,36,37,38,39,40,41
object FindBTLVote {

  /** Search through below the ine data for a particular vote */
  def search(iterator: IterateOverRawBTLData, preferences:String,blanksMatchAnything:Boolean) : SearchResult = {
    val desired = (new Splitter).split(preferences)
    val work = new SearchWork(desired)
    for (btl<-iterator) {
      var matches = 0
      for (i<-desired.indices) {
        val btlvote = if (i<btl.length) btl(i) else ""
        if ((blanksMatchAnything && desired(i).isEmpty) || desired(i)==btlvote) matches+=1
      }
      work.addMatch(matches,new VoteLine(iterator.currentRowMetadata,btl))
    }
    work
  }


}


sealed abstract class SearchResult {
  // def toHTML : NodeSeq
}

class SearchError(errorMessage:String) extends SearchResult {
  // override def toHTML: Elem = <p>Error : {errorMessage}</p>
}

class VoteLine(val details:Map[String,String],val votes:Array[String]) {
  /*
  def toHTML(lookingfor:Array[String],meta:Array[String]): Elem = {
    val res = new mutable.ArrayBuffer[NodeSeq]
    for (i<-lookingfor.indices) {
      if (i!=0) res+=Text(",")
      res+= <span class={if (votes(i)==lookingfor(i)) "match" else "mismatch"}>{votes(i)}</span>
    }
    <tr>{for (m<-meta) yield <td>{details.getOrElse(m,"")}</td>}<td>{res}</td></tr>
  }*/
}

/** A list whose length has an upper bound */
class CappedLengthList[T](val maxLength:Int,val list:List[T]=Nil,val length:Int=0) {
  def add(x: =>T) : CappedLengthList[T] = new CappedLengthList[T](maxLength,if (length<maxLength) x::list else list,length+1)
  def hasOverflow : Boolean = numOverflow>0
  def numOverflow : Int = (length-list.length) max 0
}

/** The best things seen so far */
class TopSoFar[T](val score:Int,val list:CappedLengthList[T],val next:Option[TopSoFar[T]]) {
  def trim(numTopWanted : Int) : Option[TopSoFar[T]] = if (numTopWanted>0) Some(new TopSoFar[T](score,list,next.flatMap{_.trim(numTopWanted-1)})) else None
  def add(newScore:Int,x: =>T,maxLength:Int,numTopWanted:Int) : TopSoFar[T] = {
    if (newScore>this.score) new TopSoFar(newScore,new CappedLengthList[T](maxLength).add(x),trim(numTopWanted-1))
    else if (newScore==this.score)  new TopSoFar(newScore,list.add(x),next)
    else if (numTopWanted==1) this
    else new TopSoFar(this.score,list,TopSoFar.add(next,newScore,x,maxLength,numTopWanted-1))
  }
}

class TopSoFarIterator[T](private var contents:Option[TopSoFar[T]]) extends Iterator[TopSoFar[T]] {
  override def hasNext: Boolean = contents.nonEmpty
  override def next(): TopSoFar[T] = { val res = contents.get; contents=contents.get.next; res}
}

object TopSoFar {
  def add[T](existing:Option[TopSoFar[T]],score:Int,x: =>T,maxLength:Int,numTopWanted:Int) : Option[TopSoFar[T]] = if (numTopWanted==0) None else Some{
    existing match {
      case Some(nextBest) => nextBest.add(score,x,maxLength,numTopWanted)
      case None => new TopSoFar(score,new CappedLengthList[T](maxLength).add(x),None)
    }
  }
}

class SearchWork(lookingfor:Array[String],val maxLength:Int=10,val numTopWanted:Int=3) extends SearchResult {
  var best: Option[TopSoFar[VoteLine]] = None

  def addMatch(score: Int, entry: => VoteLine): Unit = {
    best = TopSoFar.add(best, score, entry, maxLength, numTopWanted)
  }
  /*
    override def toHTML: NodeSeq = {
      val meta: Array[String] = (new TopSoFarIterator(best)).flatMap{_.list.list}.flatMap{_.details.keys}.toArray.distinct
      val it = for (level <- new TopSoFarIterator(best)) yield <div>
        <h5>Matching {level.score} entries ({lookingfor.length-level.score} mismatches)</h5>
        <table>
            {for (line <- level.list.list) yield line.toHTML(lookingfor,meta)}
            {if (level.list.hasOverflow) <tr><td rowspan={(meta.length+1).toString}>and {level.list.numOverflow} others.</td></tr> else NodeSeq.Empty}
        </table>
      </div>
      NodeSeq.seqToNodeSeq(it.toSeq)
    }*/
}

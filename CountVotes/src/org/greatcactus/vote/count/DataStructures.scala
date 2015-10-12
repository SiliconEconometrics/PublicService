/*
    Copyright 2015 Silicon Econometrics Pty. Ltd. 

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

import java.io.BufferedReader
import java.io.FileReader
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.ListBuffer
import java.io.FileOutputStream
import java.io.FileInputStream
import java.io.PrintWriter
import java.io.FileWriter


sealed class ElectionData(
    val candidates : Array[Candidate],
    val satls : Array[SATL],
    val ratls : Array[ATL],
    val btls : Array[BTL]
    ) {
  def numSATLs = satls.map{_.numVoters}.sum
  def numRATLs = ratls.map{_.numVoters}.sum
  def printStatus() {
    println("SATLs : "+numSATLs)
    println("RATLS : "+numRATLs+" num distinct = "+ratls.length)
    println("BTLs : "+btls.length)
    println("Total formal votes : "+(numSATLs+numRATLs+btls.length))
    println("Candidates : " + candidates.size)
  }
  def makeVotes : Array[Vote] = {
    val res = new scala.collection.mutable.ArrayBuffer[Vote]
    val groups : Map[String,Array[Int]] = Map.empty++ (for ((group,l)<-candidates.zipWithIndex.groupBy{ _._1.group }) yield (group,l.map{_._2}.toList.sorted.toArray))
    for (s<-satls) res+=new Vote(groups(s.group.toString),s.numVoters)
    for (s<-ratls) res+=new Vote(s.groups.flatMap{c=>groups(c.toString)},s.numVoters)
    for (b<-btls) res+=new Vote(b.candidates,1)
    res.toArray
  }
  def candidateIndex(name:String) : Int = candidates.indexWhere { _.name==name}
}

trait Dumpable {
  def line:String
}

sealed class Candidate(val name:String,val group:String,val position:Int) extends Dumpable {
  def line = name+"\t"+group+"\t"+position
}
    


class Vote(/** ordered list of candidates */ val preferences:Array[Int],val numVoters:Int)



sealed class SATL(val group:Char,val numVoters:Int) extends Dumpable {
  def line = ""+group+"\t"+numVoters
}

sealed class ATL(/** groups listed in preference order */ val groups:Array[Char],val numVoters:Int) extends Dumpable {
  def line = groups.mkString("")+"\t"+numVoters
} 

sealed class BTL(/** candidate ids listed in preference order */ val candidates:Array[Int]) extends Dumpable {
  def line = candidates.mkString(",")
}




object LoadFromFile {
  val groups = "ABCDEFGHIJKLMNOPQRSTX"
  val numGroups = groups.length
  val groupIndex : Map[Char,Int] = Map.empty++(groups.zipWithIndex)
  
  val numCandidates = 394
  val dir = "/Users/Andrew/Desktop/"
  val file = dir+"SGE2015 LC Pref Data_NA_State.txt"
  def fastfile(justIvote:Boolean) = dir+"Fast "+(if (justIvote) "ivote " else "")+"Preferences.txt"
  
  def load(justIvote:Boolean) : ElectionData = try { loadPickled(justIvote) } catch { case e:Exception => e.printStackTrace(); println("Loading manually"); val res = loadRaw(justIvote); savePickled(res,justIvote); res }
    
  /*
      implicit val upA1B = PicklerUnpickler.generate[BTL]

    implicit val upAB = PicklerUnpickler.generate[Vector[BTL]]
    implicit val upAA = PicklerUnpickler.generate[Array[ATL]]
    implicit val upAS = PicklerUnpickler.generate[Array[SATL]]
    implicit val upAC = PicklerUnpickler.generate[Array[Candidate]]
    implicit val upAED = PicklerUnpickler.generate[ElectionData]

  
  def savePickled(data:ElectionData) {
    //PicklerUnpickler.generate[SATL]
    val pickle : Array[Byte] = data.pickle.value
    println("Pickled to "+pickle.length+" bytes as "+new String(pickle))
    println("Candidates Pickled to "+data.candidates.pickle.value.length+" bytes")
    println("RATLs Pickled to "+data.ratls.pickle.value.length+" bytes")
    println("BTLs Pickled to "+data.btls.pickle.value.length+" bytes")
    val w = new FileOutputStream(fastfile)
    w.write(pickle)
    w.close()
  }
  */
  
  def savePickled(data:ElectionData,justIvote:Boolean) {
    val w = new PrintWriter(new FileWriter(fastfile(justIvote)))
    def go[T <: Dumpable](a:Array[T]) {
      w.println(a.length)
      for (e<-a) w.println(e.line)
    }
    go(data.candidates)
    go(data.satls)
    go(data.ratls)
    go(data.btls)
    w.close()  
  }
  
  def loadPickled(justIvote:Boolean) : ElectionData = {
    val r = new BufferedReader(new FileReader(fastfile(justIvote)))
    //val pickle = BinaryPickle(is)
    import scala.reflect._
    def read[T](f:String=>T)(implicit tag : ClassTag[T]) : Array[T] = {
      val len = r.readLine().toInt
      val res = new Array[T](len)
      for (i<-0 until len) res(i)=f(r.readLine())
      res
    }
    val candidates = read[Candidate]{s=>val ss = s.split('\t'); new Candidate(ss(0),ss(1),ss(2).toInt)}
    val satls = read[SATL]{s=>val ss=s.split('\t'); new SATL(ss(0).charAt(0),ss(1).toInt)}
    val ratls = read[ATL]{s=>val ss=s.split('\t'); new ATL(ss(0).toCharArray(),ss(1).toInt)}
    val btls = read[BTL]{s=>new BTL(s.split(',').map{_.toInt})}
    val res = new ElectionData(candidates,satls,ratls,btls) //  pickle.unpickle[ElectionData]
    r.close()
    res
  }
  
  def loadRaw(justIvote:Boolean) : ElectionData = {
    val r = new BufferedReader(new FileReader(file))
    var line : String = r.readLine();
    val candidates = new scala.collection.mutable.HashMap[String,Candidate]
    val btl = new scala.collection.mutable.HashMap[Int,BelowTheLineBuilder]
    val atl = new scala.collection.mutable.HashMap[Int,AboveTheLineBuilder]
    val satlCounts = new Array[Int](numGroups)
    //val satls = new ListBuffer[AboveTheLineVote]
    try { while (line!=null) {
        val fields = line.split('\t')
        if (fields.length!=12) {
          if (fields.length!=11 || fields(10)!="Informal") println("Wrong number of fields for "+line)
        }
        else if (fields(10)=="Formal" && !fields(6).isEmpty) {
          val ballotID = fields(4).toInt
          val preferenceNumber = fields(6).toInt
          def candidate() : Candidate = {
              val candidateName = fields(7)
              candidates.getOrElseUpdate(candidateName,new Candidate(candidateName,fields(8),fields(9).toInt))
          }
          if (justIvote && fields(3)!="iVote") { if (fields(11)=="BTL") candidate() }
          else fields(11) match {
            case "BTL" =>
              val v = btl.getOrElseUpdate(ballotID,new BelowTheLineBuilder(ballotID))
              v.addVote(candidate,preferenceNumber)
            case "SATL" => 
              //val v = new AboveTheLineVote(ballotID,List(groupIndex(fields(8).charAt(0))))
              //satls+=v
              satlCounts(groupIndex(fields(8).charAt(0)))+=1
            case "RATL" =>
              val groupCode = fields(8).charAt(0)
              val v = atl.getOrElseUpdate(ballotID,new AboveTheLineBuilder(ballotID))
              v.addVote(groupCode,preferenceNumber)
         }
        }
        line=r.readLine();  
    }} catch {
      case e : Exception =>
        println(line)
        e.printStackTrace()
    }
    println("SATLs : "+satlCounts.sum)
    println("RATLs : "+atl.size)
    println("BTLs : "+btl.size)
    println("candidates : "+candidates.size)
    
    def groupLT(s1:String,s2:String) : Boolean = {
      if (s1=="") false else if (s2=="") true else s1<s2
    }
    val orderedCandidates = candidates.values.toList.sortWith((c1,c2)=>groupLT(c1.group,c2.group) || (c1.group==c2.group&&c1.position<c2.position)).toArray
    val candidateToIndex : Map[Candidate,Int]=Map.empty++orderedCandidates.zipWithIndex
    val satls = for ((count,index)<-satlCounts.zipWithIndex) yield new SATL(groups(index),count)
    def canonatls(atlmap:scala.collection.mutable.HashMap[Int,AboveTheLineBuilder]) = {
      val strings = (for (v<-atlmap.values) yield v.get).toList
      val bunched = for ((s,l)<-strings.groupBy { a => a }) yield new ATL(s.toCharArray(),l.length)
      bunched.toArray
    }
    val btls = for (v<-btl.values) yield v.get(candidateToIndex)
    new ElectionData(orderedCandidates,satls.toArray,canonatls(atl),btls.toArray)
  }
}

class AboveTheLineVote(val ballotID:Int,/** groups voted for, in order */val groups:List[Int])

class AboveTheLineBuilder(val ballotID:Int) {
  val prefs:Array[Char]=new Array[Char](LoadFromFile.numGroups)
  
  def addVote(group:Char,preferenceNumber:Int) { prefs(preferenceNumber-1)=group } // LoadFromFile.groupIndex(group))=preferenceNumber}
  
  def get : String = { 
    prefs.toList.takeWhile(_ !=0).mkString("")
  }
}

class BelowTheLineBuilder(val ballotID:Int) {
  val candidates = new Array[Candidate](LoadFromFile.numCandidates)
  def addVote(candidate:Candidate,preferenceNumber:Int) { candidates(preferenceNumber-1)=candidate}
  def get(candidateIndex: Candidate=>Int) : BTL = { 
    new BTL(candidates.toList.takeWhile(_ !=null).toArray.map(candidateIndex))
  }

}
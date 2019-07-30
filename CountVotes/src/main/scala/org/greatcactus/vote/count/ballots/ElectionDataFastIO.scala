/*
    Copyright 2015-2019 Silicon Econometrics Pty. Ltd.

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


package org.greatcactus.vote.count.ballots

import java.io._

import org.greatcactus.vote.count.IOUtil

/**
  * The primary data source is the data published by the electoral commissions. This could take a while to parse.
  * For speed's sake, a cached version is saved which can then be loaded more quickly. This handles saving
  * and loading.
  */
object ElectionDataFastIO {

  private val currentCacheFormatVersion = 2

  def savePickled(data:ElectionData,file:java.io.File) {
    file.getParentFile.mkdirs()
    savePickled(data,new FileWriter(file))
  }
  def savePickled(data:ElectionData,writer: java.io.Writer) {
    val meta = data.meta
    val w = new PrintWriter(writer)
    def go[T <: Dumpable](a:Array[T],heading:String) {
      w.println(heading)
      w.println(a.length)
      for (e<-a) w.println(e.line)
    }
    w.println("Election data cache file version \t"+currentCacheFormatVersion)
    w.println(meta.electionName.line)
    go(meta.candidates,"Candidates")
    go(meta.groupInfo,"Groups")
    w.println("Official Results")
    w.println(meta.officialResults.size)
    for (e<-meta.officialResults) w.println(e.mkString(","))
    w.println("Download Location")
    w.println(meta.downloadLocation.size)
    for (e<-meta.downloadLocation) w.println(e)
    w.println("Informal\t"+data.numInformal)
    go(data.satls,"SATLs")
    go(data.ratls,"RATLs")
    go(data.btls,"BTLs")
    w.close()
  }
  def parseCommaSeparatedIntegers(s:String) : Array[Int] = s.split(',').map{_.toInt}
  def loadPickled(file:java.io.File) : ElectionData = new PickledLoader(file).loadFull()
  def loadPickledMetadata(file:java.io.File) : ElectionMetadata = new PickledLoader(file).loadMeta()
  private class PickledLoader(file:java.io.File) {
    private val r = new BufferedReader(new FileReader(file))
    private val cacheVersion = r.readLine().split('\t')(1).toInt
    if (cacheVersion!=currentCacheFormatVersion) throw new IOException("Old cache version")
    private val electionName : ElectionName = ElectionName.ofLine(r.readLine())

    import scala.reflect._
    def read[T](f:String=>T)(implicit tag : ClassTag[T]) : Array[T] = {
      val _ = r.readLine() // heading. Just for human readability
      val len = r.readLine().toInt
      val res = new Array[T](len)
      for (i<-0 until len) res(i)=f(r.readLine())
      res
    }
    private val candidates = read[Candidate]{Candidate.ofLine}
    private val groups = read[GroupInformation]{GroupInformation.ofLine}
    private val officialResults = read[Array[Int]]{parseCommaSeparatedIntegers}
    private val downloadLocations = read[String]{s=>s}
    private val meta = new ElectionMetadata(electionName,candidates,groups,officialResults.headOption,downloadLocations)

    def loadMeta() : ElectionMetadata = {r.close(); meta }
    def loadFull() : ElectionData = {
      val numInformal = r.readLine().split('\t')(1).toInt
      val satls = read[SATL]{s=>val ss=s.split('\t'); new SATL(ss(0),ss(1).toInt)}
      val ratls = read[ATL]{s=>val ss=s.split('\t'); new ATL(ss(0).split(' '),ss(1).toInt)}
      val btls = read[BTL]{s=>val ss=s.split('\t');new BTL(parseCommaSeparatedIntegers(ss(0)),ss(1).toInt)}
      val res = new ElectionData(meta,satls,ratls,btls,numInformal)
      r.close()
      res
    }
  }
}



trait Dumpable {
  def line:String
}

/** A cache wrapper for the loadRaw function, producing a cached load function */
abstract class CachedElectionDataLoader(/** The sub-directory containing the data files. Also used for cache files. e.g. "Federal/2013" */dir:String) {
  private val cacheDir = new File(IOUtil.baseDir, "Cache/"+dir)
  val sourceDir : File = new File(IOUtil.baseDir, "Elections/"+dir)
  def rel(f: String) = new File(sourceDir, f)

  /** Do the actual work of loading election data from the EC provided data files. Files should be in directory sourceDir, accessible by function rel */
  def loadRaw(state: String) : ElectionData

  /** Load a cached version of the data. If not present, load from original and store cached version. */
  def load(state: String): ElectionData = {
    val cache = new File(cacheDir,state + ".txt")
    try {
      ElectionDataFastIO.loadPickled(cache)
    } catch {
      case e: Exception =>
        if (!e.isInstanceOf[FileNotFoundException]) e.printStackTrace()
        println("Loading manually")
        val res = loadRaw(state)
        ElectionDataFastIO.savePickled(res, cache)
        res
    }
  }

  /** Load just the metadata */
  def loadJustMetadata(state: String): ElectionMetadata = {
    val cache = new File(cacheDir,state + ".txt")
    try {
      ElectionDataFastIO.loadPickledMetadata(cache)
    } catch {
      case e: Exception =>
        // e.printStackTrace()
        println("Loading manually")
        val res = loadRaw(state)
        ElectionDataFastIO.savePickled(res, cache)
        res.meta
    }
  }
}
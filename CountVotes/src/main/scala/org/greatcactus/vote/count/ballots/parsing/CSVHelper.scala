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

package org.greatcactus.vote.count.ballots.parsing

import java.io._
import java.util.zip.ZipFile

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer


object CSVHelper {
  def fromIS(stream:InputStream,headingLines:Int,separator:Char=',',charEncoder:String="UTF8") = new CSVHelper(stream,headingLines,separator,charEncoder)
  def apply(file:File,headingLines:Int,separator:Char=',',charEncoder:String="UTF8") = new CSVHelper(new FileInputStream(file),headingLines,separator,charEncoder)
  /** Extract from a file in a zipfile, containing the string innerfilename in the name somewhere. */
  def fromZipFile(zipedFile:File,innerfilename:String,headingLines:Int) : CSVHelper = {
    val zipFile = new ZipFile(zipedFile)
    import collection.JavaConverters._
    zipFile.entries().asScala.find { _.getName.contains(innerfilename) } match {
      case Some(entry) => new CSVHelper(zipFile.getInputStream(entry),headingLines)
      case None => throw new Exception("Could not find "+innerfilename+" in "+zipedFile)
    }
  }
}

/**
  * Utility to help with interpreting a CSV file. Create, and then use as an iterator over lines - see foreach function
  * @param stream file to load.
  * @param headingLines Number of lines of headings to remove from the top.
  * @param separator Character separating fields. Default Comma for csv.
  * @param charEncoder Character set. Default UTF8
  */
class CSVHelper(stream:InputStream,headingLines:Int,separator:Char=',',charEncoder:String="UTF8") {
  val reader = new BufferedReader(new InputStreamReader(stream,charEncoder))
  private val rawHeadings = new ArrayBuffer[String]
  private def nextHeading() : String = { val h=reader.readLine(); rawHeadings+=h; h }
  val splitter = new Splitter(separator)
  val splitHeadings : immutable.IndexedSeq[Array[String]] = for (i<-0 until headingLines) yield splitter.split(nextHeading(),() => nextHeading())
  val headings: Array[String] = rawHeadings.toArray
  def splitHeading(headingNumber:Int) : Array[String] =splitHeadings(headingNumber)
  def readSingleLine() : Array[String] = splitter.split(reader.readLine(),() => reader.readLine())
  var lineCount: Int = headingLines
  //val headings = reader.readLine.split(separator)
  /** Call function f for each line in the file with separated and unescaped fields. */
  def foreach(f:Array[String]=>Unit) {
    var line = reader.readLine()
    try {
      while (line!=null) {
        lineCount+=1
        // val split = line.split(separator)
        f(splitter.split(line,() => reader.readLine()))
        line = reader.readLine()
      }
      reader.close()
    } catch { case e:Exception => println("Error reading line number "+lineCount+" : "+line); throw e; }
  }
}
/** Non thread safe helper to split a line (e.g. csv) into components, allowing for quotes */
class Splitter(separator:Char=',') {
  val split = new ArrayBuffer[String]
  val sb = new StringBuilder

  /** Non-thread safe */
  def split(line:String) : Array[String] = {
    var inquotes = false
    for (c<-line) {
      if (c=='"' && (sb.isEmpty || inquotes)) inquotes= !inquotes
      else if (c==','&& !inquotes) { split+=sb.toString; sb.clear() }
      else sb+=c
    }
    split+=sb.toString; sb.clear()
    val res = split.toArray
    split.clear()
    res
  }
  /** Non-thread safe */
  def split(firstLine:String,getContinuationLines: ()=>String) : Array[String] = {
    var inquotes = false
    def proc(line:String): Unit = {
      for (c<-line) {
        if (c=='"') inquotes= !inquotes
        else if (c==','&& !inquotes) { split+=sb.toString; sb.clear() }
        else sb+=c
      }
    }
    proc(firstLine)
    while (inquotes) {
      val nextline = getContinuationLines()
      if (nextline==null) throw new IOException("Quotation never closed before file finished")
      proc("\n"+nextline)
    }
    split+=sb.toString; sb.clear()
    val res = split.toArray
    split.clear()
    res
  }
}

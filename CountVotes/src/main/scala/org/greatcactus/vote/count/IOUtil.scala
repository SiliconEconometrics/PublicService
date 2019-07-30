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

package org.greatcactus.vote.count

import java.io.File
import java.io.FileOutputStream
import java.io.FileInputStream
import java.io.InputStream

object IOUtil {

  val devBaseDir = new java.io.File("../CountPreferentialVotes") // hack for my setup. Sorry.

  var baseDir = if (devBaseDir.isDirectory) devBaseDir else new File(".")

   def saveFile(file:File,contents:Array[Byte]) {
        file.getParentFile.mkdirs()
        val os = new FileOutputStream(file)
        os.write(contents)
        os.close()
    
  }
  
  def copyFile(copyFrom:File,copyTo:File) {
    saveFile(copyTo,loadFile(copyFrom))
  }
  def loadFile(file:File) : Array[Byte] = {
    val res = new Array[Byte](file.length().toInt)
    val is = new FileInputStream(file)
    var upto = 0
    while (upto<res.length) {
      val len = is.read(res,upto,res.length-upto)
      upto+=len
    }
    is.close()
    res
  }
  def loadFileAsString(file:File) : String = new String(loadFile(file))
  
  def loadStream(s:InputStream) : Array[Byte] = {
    // sun.misc.IOUtils.readFully(s, -1, true)
    // s.readAllBytes() // Java9 standard version.
    val out = new java.io.ByteArrayOutputStream
    var nRead = 0
    val buffer = new Array[Byte](16384)
    while ({nRead = s.read(buffer, 0, buffer.length); nRead != -1}) out.write(buffer, 0, nRead)
    return out.toByteArray
  }

  def loadResource(name:String) : Array[Byte] = {
    val stream: InputStream = getClass.getResourceAsStream("/"+name)
    try {
      loadStream(stream)
    } finally { stream.close() }
  }
}
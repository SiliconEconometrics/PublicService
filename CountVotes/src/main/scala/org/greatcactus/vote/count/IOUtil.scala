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

package org.greatcactus.vote.count

import java.io.File
import java.io.FileOutputStream
import java.io.FileInputStream
import java.io.InputStream

object IOUtil {

  var baseDir = new File(".")


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
    sun.misc.IOUtils.readFully(s, -1, true)
  }
  
}
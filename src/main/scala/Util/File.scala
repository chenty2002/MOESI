package Util

import java.io.{File, FileWriter}

object File {
  def writeOutputFile(targetDir: String, fname: String, contents: String): File = {
    val f = new File(targetDir, fname)
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close()
    f
  }
}

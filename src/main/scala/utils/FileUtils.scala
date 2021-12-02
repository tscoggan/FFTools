package fftools
package utils

import java.io.{ File, FileWriter }
import scala.io.Source
import scala.util.Using
import scala.collection.mutable

object FileUtils {

  def readFile(fileName: String): List[String] = {
    Using(Source.fromFile(fileName)) { source => source.getLines().toList }.get // throws any exception that occurs
  }

  /**
   * Returns a list of all files in the specifier directory with the specified extension(s).  If no extensions are
   * specified, all files are returned.
   */
  def getListOfFiles(dir: String, extensions: String*): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      if (extensions.isEmpty) d.listFiles.filter(_.isFile).toList
      else {
        d.listFiles.filter(_.isFile).toList.filter { f =>
          extensions.toList.exists { e => f.getName.toUpperCase.endsWith(e.toUpperCase) }
        }
      }
    } else Nil
  }

  def getSubdirectories(parentDir: String): List[File] = {
    val d = new File(parentDir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isDirectory).toList
    } else Nil
  }

  // key: (filename, overwrite existing file when writing?)
  private val fileWriterCache: mutable.Map[(String, Boolean), FileWriter] = mutable.Map.empty

  private def getFileWriter(fileName: String, overwrite: Boolean): FileWriter = synchronized {
    fileWriterCache.get((fileName, overwrite)) match {
      case Some(cached) => cached
      case None => {
        //log(s"$fileName file writer not found in cache --> adding")
        val fw = new FileWriter(fileName, !overwrite)
        fileWriterCache += ((fileName, overwrite) -> fw)
        fw
      }
    }
  }

  def writeToFile(s: String, fileName: String, overwrite: Boolean = false): Unit = {
    val file = new File(fileName)
    file.getParentFile.mkdirs
    val writer = getFileWriter(fileName, overwrite)
    writer.write(s)
    writer.flush()
  }

  def writeLinesToFile(lines: Iterable[String], fileName: String, overwrite: Boolean = false): Unit = {
    val file = new File(fileName)
    file.getParentFile.mkdirs
    if (overwrite && file.exists) file.delete
    val writer = getFileWriter(fileName, overwrite)
    writer.write(lines.mkString("\n") + "\n")
    writer.flush()
  }

  def fileExists(fileName: String): Boolean = (new File(fileName)).exists

  //noinspection ConvertibleToMethodValue
  def delete(file: File): Unit = {
    if (file.isDirectory) file.listFiles.foreach(delete(_))
    if (file.exists && !file.delete) throw new Exception(s"Unable to delete ${file.getAbsolutePath}")
  }

}

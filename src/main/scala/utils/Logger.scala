package fftools
package utils

object Logger {

  def log(text: String): Unit = println(text)

  def logDebug(text: String): Unit = if (Configs.logDebug) log(text)

}

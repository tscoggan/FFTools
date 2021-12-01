package fftools
package utils

import com.typesafe.config.{ConfigFactory, Config}

object Configs {
  private val conf = ConfigFactory.load.getConfig("utils")

  val logDebug: Boolean = conf.getBoolean("log_debug")
}

package fftools
package utils

import scala.collection.mutable
import Logger._

class Cache[T] {
  val cache: mutable.ListBuffer[T] = mutable.ListBuffer.empty

  def add(t: T): Unit = synchronized {
    cache += t
    logDebug(s"Added to cache (new size = ${cache.size}): " + t)
  }

  def remove(t: T): Unit = synchronized {
    if (cache.contains(t)) {
      cache -= t
      logDebug(s"Removed from cache (new size = ${cache.size}): " + t)
    } else log(s"WARNING: Cannot remove from cache, not found: " + t)
  }
}

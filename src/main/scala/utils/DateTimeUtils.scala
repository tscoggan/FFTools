package fftools
package utils

import java.util.{ Date, Calendar }
import org.joda.time.DateTime
import java.text.SimpleDateFormat
import scala.collection.mutable
import scala.annotation.tailrec
import Logger._

object DateTimeUtils {

  def today: Date = Calendar.getInstance.getTime

  def yesterday: Date = today.previousDay

  def oneYearAgo: Date = today.minusDays(365)

  def timeInSeconds[T](code: => T): (T, Int) = {
    val t1 = System.currentTimeMillis
    val result = code
    val elapsed = ((System.currentTimeMillis - t1) / 1000).toInt
    (result, elapsed)
  }

  private val dateFormatCache: mutable.Map[String, SimpleDateFormat] = mutable.Map.empty

  def getDateFormat(format: String): SimpleDateFormat = dateFormatCache.get(format) match {
    case Some(cached) => cached
    case None =>
      log(s"Date format $format not found in cache --> adding")
      val df = new SimpleDateFormat(format)
      dateFormatCache += (format -> df)
      df
  }

  def getDatesBetween(start: Date, end: Date, inclusive: Boolean = true): List[Date] = {
    @tailrec def next(dates: List[Date], currentDay: Date, lastDay: Date): List[Date] = currentDay.trimTime.isAfter(lastDay.trimTime) match {
      case true  => dates.sorted
      case false => next(currentDay :: dates, currentDay.nextDay, lastDay)
    }

    inclusive match {
      case true  => next(Nil, start, end)
      case false => next(Nil, start.nextDay, end.previousDay)
    }
  }

  implicit class EnrichedDate(d: Date) {

    def print(format: String = "yyyy-MM-dd"): String = getDateFormat(format).format(d)

    def plusDays(days: Int): Date = (new DateTime(d)).plusDays(days).toDate

    def nextDay: Date = d.plusDays(1)

    def minusDays(days: Int): Date = (new DateTime(d)).minusDays(days).toDate

    def previousDay: Date = d.minusDays(1)

    def isBefore(other: Date): Boolean = (new DateTime(d)).isBefore(new DateTime(other))

    def isAfter(other: Date): Boolean = (new DateTime(d)).isAfter(new DateTime(other))

    //noinspection RedundantDefaultArgument
    def isSameDayAs(other: Date): Boolean = {
      d.print("yyyy-MM-dd") == other.print("yyyy-MM-dd")
    }

    def trimTime: Date = {
      val calendar = Calendar.getInstance
      calendar.setTime(d)
      calendar.set(Calendar.HOUR_OF_DAY, 0)
      calendar.set(Calendar.MINUTE, 0)
      calendar.set(Calendar.SECOND, 0)
      calendar.set(Calendar.MILLISECOND, 0)
      calendar.getTime
    }

    def year: Int = {
      val calendar = Calendar.getInstance
      calendar.setTime(d)
      calendar.get(Calendar.YEAR)
    }

  }

}

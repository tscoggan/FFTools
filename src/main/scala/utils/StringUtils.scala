package fftools
package utils

import scala.annotation.tailrec
import java.util.Date

object StringUtils {

  implicit class EnrichedString(s: String) {

    /**
     * Ignores commas surrounded by quotation marks.  For example, invoking this method on the following string...
     * {{{ a,b,"c,d",e }}}
     * ...will produce:
     * {{{ Array("a", "b", "c,d", "e") }}}
     */
    def splitCSV(discardOuterQuotes: Boolean = true): Array[String] = {
      val tokens = s.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1) // 2nd param ensures trailing empty tokens are included
      if (discardOuterQuotes) tokens.map(_.trimPrefix("\"").trimSuffix("\""))
      else tokens
    }

    /**
     * Ignores spaces surrounded by quotation marks.  For example, invoking this method on the following string...
     * {{{ a b "c d" e }}}
     * ...will produce:
     * {{{ Array("a", "b", "c d", "e") }}}
     */
    def splitOnSpace(discardOuterQuotes: Boolean = true): Array[String] = {
      val tokens = s.split("\\s+(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1) // 2nd param ensures trailing empty tokens are included
      if (discardOuterQuotes) tokens.map(_.trimPrefix("\"").trimSuffix("\""))
      else tokens
    }

    def trimPrefix(prefix: String): String = {
      if (s.startsWith(prefix)) s.substring(prefix.length)
      else s
    }

    def trimSuffix(suffix: String): String = {
      if (s.endsWith(suffix)) s.substring(0, s.length - suffix.length)
      else s
    }

    def substringBefore(delim: String): String = s.indexOf(delim) match {
      case -1 => s // delim not found
      case i  => s.substring(0, i)
    }

    def substringAfter(delim: String): String = s.indexOf(delim) match {
      case -1 => s // delim not found
      case i  => s.substring(i + delim.length)
    }

    def substringAfterLast(delim: String): String = s.lastIndexOf(delim) match {
      case -1 => s // delim not found
      case i  => s.substring(i + delim.length)
    }

    def substringBeforeLast(delim: String): String = s.lastIndexOf(delim) match {
      case -1 => s // delim not found
      case i  => s.substring(0, i)
    }

    /**
     * Returns a list of all substrings enclosed by the specified `prefix` and `suffix`.
     * @example {{{
     *    "ab(cd)e(f)gh".substringsBetween("("),")")  // returns List("cd", "f")
     * }}}
     */
    def substringsBetween(prefix: String, suffix: String): List[String] = {
      @tailrec def next(results: List[String], remaining: String): List[String] = remaining.length match {
        case 0 => results.reverse
        case _ =>
          val prefixStart = remaining.indexOf(prefix)
          val suffixStart = remaining.indexOf(suffix, prefixStart + prefix.length)
          if (prefixStart >= 0 && suffixStart > prefixStart) {
            next(remaining.substring(prefixStart + prefix.length, suffixStart) :: results, remaining.substring(suffixStart + suffix.length))
          } else results.reverse
      }
      next(Nil, s)
    }

    def toDate(format: String = "yyyy-MM-dd"): Date = DateTimeUtils.getDateFormat(format).parse(s)

  }

}

package fftools
package utils

object FloatUtils {

  implicit class EnrichedFloat(f: Float) {

    /** Returns `true` if the two numbers are equal using the specified precision */
    def ~=(other: AnyVal, precision: Float = 0.0001f): Boolean = other match {
      case x: Float  => if ((f - x).abs < precision) true else false
      case x: Double => if ((f - x).abs < precision) true else false
      case x: Int    => if ((f - x).abs < precision) true else false
      case x: Long   => if ((f - x).abs < precision) true else false
      case x: Short  => if ((f - x).abs < precision) true else false
      case x: Byte   => if ((f - x).abs < precision) true else false
    }

    def rounded(decimalPlaces: Int): String = {
      val df = new java.text.DecimalFormat("#." + ("#" * decimalPlaces))
      df.format(f)
    }

  }

}

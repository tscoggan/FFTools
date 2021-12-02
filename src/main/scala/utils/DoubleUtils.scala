package fftools
package utils

object DoubleUtils {

  implicit class EnrichedDouble(d: Double) {

    /** Returns `true` if the two numbers are equal using the specified precision */
    def ~=(other: AnyVal, precision: Double = 0.0001): Boolean = other match {
      case x: Float  => if ((d - x).abs < precision) true else false
      case x: Double => if ((d - x).abs < precision) true else false
      case x: Int    => if ((d - x).abs < precision) true else false
      case x: Long   => if ((d - x).abs < precision) true else false
      case x: Short  => if ((d - x).abs < precision) true else false
      case x: Byte   => if ((d - x).abs < precision) true else false
    }

    def rounded(decimalPlaces: Int): String = {
      val df = new java.text.DecimalFormat("#." + ("#" * decimalPlaces))
      df.format(d)
    }

  }

}

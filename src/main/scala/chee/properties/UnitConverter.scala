package chee.properties

final class UnitConverter(factors: List[(String, Int)], idUnit: String, errmsg: String) {
  import java.text._

  private val factorMap = factors.toMap.withDefaultValue(1)
  private val unitChars = factors.map(_._1).mkString.distinct
  private val sizeRegex = ("""(?i)([0-9]+(\.[0-9]+)?)""" + s"([$unitChars]+)?").r

  def convert(number: Long): Option[(Double, String)] =
    factors.map({ case (unit, f) =>
      (number.toDouble / f.toDouble, unit)
    }).find(_._1 > 1.0)

  private def formatter =
    new DecimalFormat("#.#", new DecimalFormatSymbols(java.util.Locale.ROOT))

  def render(number: Long): String = {
    convert(number) match {
      case Some((value, unit)) =>
        formatter.format(value) + unit
      case None =>
        s"$number$idUnit"
    }
  }

  def parse(str: String): Either[String, Long] = str match {
    case sizeRegex(digits, _, null) =>
      Right(digits.toLong)

    case sizeRegex(digits, _, `idUnit`) =>
      Right(digits.toLong)

    case sizeRegex(digits, _, unit) =>
      val un = (if (unit.length != 2) s"$unit$idUnit" else unit).toLowerCase
      factorMap.get(un) match {
        case Some(f) => Right((digits.toDouble * f).toLong)
        case _ => Left(s"$errmsg: $str")
      }

    case _ => Left(s"$errmsg: $str")
  }
}

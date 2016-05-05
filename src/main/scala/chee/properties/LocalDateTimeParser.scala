package chee.properties

import fastparse.all._
import chee.util.parsing._
import LocalDateTime._
import LocalDateTimeParser._

class LocalDateTimeParser(now: LocalDateTime, atEnd: Boolean) {

  lazy val fullDateTime: P[LocalDateTime] = P(fullSpec.map {
    case (y, Seq(m, d, h, min, s)) =>
      LocalDateTime(y, m, d, h, min, s)
    case (y, rest) =>
      rest.map(n => Some(n)) ++ Seq.fill(4 - rest.size)(None) match {
        case Seq(om, od, oh, omin) =>
          if (atEnd) LocalDateTime.atEnd(y, om, od, oh, omin)
          else LocalDateTime.atStart(y, om, od, oh, omin)
        case _ => sys.error("unreachable code")
      }
  })

  lazy val monthDay: P[LocalDateTime] =
    P(oneOrTwoDigits ~ noDigit ~ oneOrTwoDigits).map {
      case (m, d) =>
        val date =
          if (atEnd) LocalDateTime.atEnd(now.year, Some(m), Some(d))
          else LocalDateTime.atStart(now.year, Some(m), Some(d))
        if (date > now) date - 1.year else date
    }

  lazy val day: P[LocalDateTime] =
    P(oneOrTwoDigits).map { d =>
      val date =
        if (atEnd) LocalDateTime.atEnd(now.year, Some(now.month), Some(d))
        else LocalDateTime.atStart(now.year, Some(now.month), Some(d))
      if (date > now) date - 1.month else date
    }

  lazy val eastersun: P[LocalDateTime] = P(fourDigits ~ noDigit ~ ic("eastersun")).map {
    case year => LocalDateTime.eastersun(year, atEnd)
  }

  lazy val ascension: P[LocalDateTime] = P(fourDigits ~ noDigit ~ ic("ascension")).map {
    case year => LocalDateTime.eastersun(year, atEnd) + 39.days
  }

  lazy val whitsun: P[LocalDateTime] =  P(fourDigits ~ noDigit ~ ic("whitsun")).map {
    case year => LocalDateTime.eastersun(year, atEnd) + 49.days
  }

  lazy val nowLiteral: P[LocalDateTime] = ic("now").map(_ => now)

  lazy val dateTimeKeys: P[LocalDateTime] =
    eastersun | ascension | whitsun | nowLiteral

  lazy val dateTimeFix: P[LocalDateTime] =
    dateTimeKeys | fullDateTime | monthDay | day

  lazy val dateTimeCalc: P[LocalDateTime] = P((dateTimeFix ~ "#").? ~ signedTimeAmount).map {
    case (dt, amount) =>
      (dt.getOrElse(now) + amount).resetTime(amount)
  }

  lazy val dateTime: P[LocalDateTime] = dateTimeCalc | dateTimeFix

  def parseDate(str: String): Either[String, LocalDateTime] =
    dateTime.parseAll(str)
}

object LocalDateTimeParser {
  private val digit = CharIn('0' to '9')

  lazy val digits: P[Int] = P(digit.rep(1).!).map(_.toInt)
  lazy val oneOrTwoDigits: P[Int] = P(digit.rep(min = 1, max = 2).!).map(_.toInt)
  lazy val fourDigits: P[Int] = P(digit.times(4).!).map(_.toInt)
  lazy val noDigit: P0 = P(!digit ~ AnyChar)

  lazy val fullSpec = P(fourDigits ~ //year
    //month, day, hour, minute, second
    (noDigit ~ oneOrTwoDigits).rep(min = 0, max = 5))

  lazy val year: P[Int => TimeAmount] = ic("y").map(_ => n => n.toInt.years)
  lazy val mon: P[Int => TimeAmount] = ic("m").map(_ => n => Months(n))
  lazy val day: P[Int => TimeAmount] = ic("d").map(_ => n => Days(n))
  lazy val hour: P[Int => TimeAmount] = ic("h").map(_ => n => Hours(n))
  lazy val min: P[Int => TimeAmount] = ic("min").map(_ => n => Minutes(n))
  lazy val sec: P[Int => TimeAmount] = ic("s").map(_ => n => Seconds(n))

  lazy val timeUnit: P[Int => TimeAmount] =
    P(min|year|mon|day|hour|sec)

  lazy val timeAmount: P[TimeAmount] = P(digits ~ timeUnit.?).map {
    case (num, Some(unit)) => unit(num)
    case (num, None) => Days(num)
  }

  lazy val signedTimeAmount: P[TimeAmount] =
    P(("+"|"-").! ~ digits ~ timeUnit.?).map {
      case (sign, num, unit) =>
        val n = if (sign == "-") -num else num
        val unitf = unit getOrElse (Days.apply _)
        unitf(n)
    }
}

class LocalDateTimeRangeParser(now: LocalDateTime) {

  private val dateP = new LocalDateTimeParser(now, false)

  lazy val simpleRange: Parser[DateRange] = P(dateP.dateTime ~ "--" ~ dateP.dateTime).map {
    (DateRange.apply _).tupled
  }

  lazy val calcRange = P(dateP.dateTime ~ "--" ~ signedTimeAmount).map {
    case (start, amount) =>
      if (amount.n > 0) DateRange(start, start + amount)
      else DateRange(start - amount.abs, start)
  }

  val dateRange: Parser[DateRange] = calcRange | simpleRange

  def parseRange(str: String): Either[String, DateRange] =
    dateRange.parseAll(str.trim)
}

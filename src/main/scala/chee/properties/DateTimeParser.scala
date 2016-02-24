package chee.properties

import scala.util.parsing.combinator.RegexParsers

class DateTimeParser(now: LocalDateTime = LocalDateTime.now) extends RegexParsers {
  import LocalDateTime._

  override val whiteSpace = "".r

  val fourDigits: Parser[Int] = "[0-9]{4}".r ^^ { _.toInt }
  val twoDigits: Parser[Int] = "[0-9]{2}".r ^^ { _.toInt }
  val oneOrTwoDigits: Parser[Int] = "[0-9]{1,2}".r ^^ { _.toInt }
  val digits: Parser[Long] = "[0-9]+".r ^^ { _.toLong }
  val noDigit: Parser[String] = "[^0-9+]".r

  val fullSpec = fourDigits ~
    (noDigit ~> oneOrTwoDigits).? ~
    (noDigit ~> oneOrTwoDigits).? ~
    (noDigit ~> oneOrTwoDigits).? ~
    (noDigit ~> oneOrTwoDigits).? ~
    (noDigit ~> oneOrTwoDigits).?

  def fullDateTime(atEnd: Boolean): Parser[LocalDateTime] = fullSpec ^^ {
    case y ~ Some(m) ~ Some(d) ~ Some(h) ~ Some(min) ~ Some(s) =>
      LocalDateTime(y, m, d, h, min, s)
    case y ~ om ~ od ~ oh ~ omin ~ _ =>
      if (atEnd) LocalDateTime.atEnd(y, om, od, oh, omin)
      else LocalDateTime.atStart(y, om, od, oh, omin)
  }

  def monthDay(atEnd: Boolean): Parser[LocalDateTime] =
    oneOrTwoDigits ~ (noDigit ~> oneOrTwoDigits) ^^ {
      case m ~ d =>
        val date =
          if (atEnd) LocalDateTime.atEnd(now.year, Some(m), Some(d))
          else LocalDateTime.atStart(now.year, Some(m), Some(d))
        if (date > now) date - 1.year
        else date
    }

  def day(atEnd: Boolean): Parser[LocalDateTime] = oneOrTwoDigits ^^ {
    case d =>
      val date =
        if (atEnd) LocalDateTime.atEnd(now.year, Some(now.month), Some(d))
        else LocalDateTime.atStart(now.year, Some(now.month), Some(d))
      if (date > now) date - 1.month
      else date
  }

  def eastersun(atEnd: Boolean): Parser[LocalDateTime] = fourDigits <~ (noDigit ~> "(?i)eastersun".r) ^^ {
    case year => LocalDateTime.eastersun(year, atEnd)
  }

  def ascension(atEnd: Boolean): Parser[LocalDateTime] = fourDigits <~ (noDigit ~> "(?i)ascension".r) ^^ {
    case year => LocalDateTime.eastersun(year, atEnd) + 39.days
  }

  def whitsun(atEnd: Boolean): Parser[LocalDateTime] =  fourDigits <~ (noDigit ~> "(?i)whitsun".r) ^^ {
    case year => LocalDateTime.eastersun(year, atEnd) + 49.days
  }

  def timeAmount: Parser[TimeAmount] = digits ~ opt("y"|"m"|"d"|"h"|"min"|"s") ^^ {
    case num ~ Some(unit) => (num, unit.toLowerCase) match {
      case (n, "y") => Years(n.toInt)
      case (n, "m") => Months(n)
      case (n, "d") => Days(n)
      case (n, "h") => Hours(n)
      case (n, "min") => Minutes(n)
      case (n, "s") => Seconds(n)
    }
    case num ~ None => Days(num)
  }

  val nowLiteral: Parser[LocalDateTime] = "(?i)now".r ^^ { _ => now }

  def dateTimeKeys(atEnd: Boolean): Parser[LocalDateTime] =
    eastersun(atEnd) | ascension(atEnd) | whitsun(atEnd) | nowLiteral

  def dateTimeFix(atEnd: Boolean) =
    dateTimeKeys(atEnd) | fullDateTime(atEnd) | monthDay(atEnd) | day(atEnd)

  def dateTimeCalc(atEnd: Boolean)= opt(dateTimeFix(atEnd) <~ "#") ~ ("+"|"-") ~ timeAmount ^^ {
    case dt ~ "+" ~ amount => resetTime(dt.getOrElse(now) + amount, amount)
    case dt ~ "-" ~ amount => resetTime(dt.getOrElse(now) - amount, amount)
  }

  def resetTime(dt: LocalDateTime, amount: TimeAmount): LocalDateTime =
    amount match {
      case _: Hours => dt.copy(minute = 0, second = 0)
      case _: Minutes => dt.copy(second = 0)
      case _: Seconds => dt
      case _ => dt.copy(hour = 0, minute = 0, second = 0)
    }

  def dateTime(atEnd: Boolean) = dateTimeCalc(atEnd) | dateTimeFix(atEnd)

  val simpleRange: Parser[DateRange] = dateTime(false) ~ ("--" ~> dateTime(false)) ^^ {
    case dt1 ~ dt2 => DateRange(dt1, dt2)
  }

  val calcRange = (dateTime(false) <~ "--") ~ ("+"|"-") ~ timeAmount ^^ {
    case start ~ "+" ~ amount => DateRange(start, start + amount)
    case start ~ "-" ~ amount => DateRange(start - amount, start)
  }

  def dateRange: Parser[DateRange] = calcRange | simpleRange

  def parseDate(str: String, atEnd: Boolean): Either[String, LocalDateTime] =
    parseAll(dateTime(atEnd), str.trim) match {
      case Success(d, _) => Right(d)
      case f => Left(f.toString)
    }
}

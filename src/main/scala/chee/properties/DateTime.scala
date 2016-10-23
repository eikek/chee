package chee.properties

import java.time._
import java.time.format._
import java.time.{LocalDateTime => JLD}

case class DateTime(instant: Long) {
  def < (o: DateTime): Boolean = instant < o.instant
  def > (o: DateTime): Boolean = instant > o.instant

  def format(pattern: String, zone: ZoneId = ZoneId.systemDefault): String = {
    val formatter = DateTimeFormatter.ofPattern(pattern)
    val dt = Instant.ofEpochMilli(instant).atZone(zone)
    dt.format(formatter)
  }
}

object DateTime {
  def apply(instant: java.time.Instant): DateTime = DateTime(instant.toEpochMilli())
  def now: DateTime = apply(Instant.now())
}

case class LocalDateTime(
  year: Int,
  month: Int,
  day: Int,
  hour: Int,
  minute: Int,
  second: Int) {

  require(month >= 1 && month <= 12, s"invalid month: $month")
  require(day >= 1 && day <= 31, s"invalid day: $day")
  require(hour >= 0 && hour <= 23, s"invalid hour: $hour")
  require(minute >= 0 && minute <= 59, s"invalid minute: $minute")
  require(second >= 0 && second <= 59, s"invalid second: $second")

  import LocalDateTime._

  val values = List(year, month, day, hour, minute, second)

  def < (o: LocalDateTime): Boolean =
    values.zip(o.values)
      .collect({case (a, b) if a != b => a < b})
      .headOption
      .getOrElse(false)

  def > (o: LocalDateTime) = o != this && !(this < o)

  def asJava: JLD = JLD.of(year, Month.of(month), day, hour, minute, second)

  def + (t: TimeAmount): LocalDateTime = t.n match {
    case 0 => this
    case n if n < 0 => this - t.abs
    case _ => t match {
      case Years(n) => copy(year = year + n)
      case Months(n) => fromJava(asJava.plusMonths(n.toLong))
      case Days(n) => fromJava(asJava.plusDays(n.toLong))
      case Hours(n) => fromJava(asJava.plusHours(n.toLong))
      case Minutes(n) => fromJava(asJava.plusMinutes(n.toLong))
      case Seconds(n) => fromJava(asJava.plusSeconds(n.toLong))
    }
  }

  def - (t: TimeAmount): LocalDateTime = t.n match {
    case 0 => this
    case n if n < 0 => this + t.abs
    case _ => t match {
      case Years(n) => copy(year = year - n)
      case Months(n) => fromJava(asJava.minusMonths(n.toLong))
      case Days(n) => fromJava(asJava.minusDays(n.toLong))
      case Hours(n) => fromJava(asJava.minusHours(n.toLong))
      case Minutes(n) => fromJava(asJava.minusMinutes(n.toLong))
      case Seconds(n) => fromJava(asJava.minusSeconds(n.toLong))
    }
  }

  def resetTime(amount: TimeAmount): LocalDateTime =
    amount match {
      case _: Hours => copy(minute = 0, second = 0)
      case _: Minutes => copy(second = 0)
      case _: Seconds => this
      case _ => copy(hour = 0, minute = 0, second = 0)
    }

  def toDateTime(zone: ZoneId = ZoneId.systemDefault): DateTime =
    DateTime(asJava.atZone(zone).toInstant)

  def format(pattern: String): String = {
    val formatter = DateTimeFormatter.ofPattern(pattern)
    val dt = JLD.of(year, month, day, hour, minute, second)
    dt.format(formatter)
  }

  def asString = f"$year-$month%02d-$day%02d $hour%02d:$minute%02d:$second%02d"
}

object LocalDateTime {
  def now = fromJava(JLD.now)

  def atEnd(year: Int,
    month: Option[Int] = None,
    day: Option[Int] = None,
    hour: Option[Int] = None,
    minute: Option[Int] = None): LocalDateTime = {
    val r = LocalDateTime(year, month.getOrElse(12), day.getOrElse(31), hour.getOrElse(23), minute.getOrElse(59), 59)
    if (day == None) {
      val jyear = Year.of(year)
      val jmon = Month.of(month.getOrElse(12))
      r.copy(day = jmon.length(jyear.isLeap))
    } else r
  }

  def atStart(year: Int,
    month: Option[Int] = None,
    day: Option[Int] = None,
    hour: Option[Int] = None,
    minute: Option[Int] = None): LocalDateTime =
    LocalDateTime(year, month.getOrElse(1), day.getOrElse(1), hour.getOrElse(0), minute.getOrElse(0), 0)

  def fromJava(jld: JLD): LocalDateTime =
    LocalDateTime(jld.getYear, jld.getMonth.getValue, jld.getDayOfMonth, jld.getHour, jld.getMinute, jld.getSecond)

  /** see wikipedia */
  def eastersun(year: Int, end: Boolean = false) = {
    val a = year % 19
    val b = (year / 100).toInt
    val c = year % 100
    val d = (b / 4).toInt
    val e = b % 4
    val f = ((b + 8) / 25).toInt
    val g = (((b - f) +1) / 3).toInt
    val h = ((19 * a) + b + (-1 * d) + (-1 * g) + 15) % 30
    val i = (c / 4).toInt
    val k = c % 4
    val l = (32 + (2 * e) + (2 * i) + (-1 * h) + (-1 * k)) % 7
    val m = (a + (11 * h) + (22 * l)) / 451
    val mon = (h + l + (-7 * m) + 114) / 31
    val day = ((h + l + (-7 * m) + 114) % 31) + 1
    if (end) atEnd(year, Some(mon), Some(day))
    else atStart(year, Some(mon), Some(day))
  }

  sealed trait TimeAmount {
    val n: Int
    def abs: TimeAmount = map(_.abs)
    // reduce this boilerplate with shapless
    // http://www.cakesolutions.net/teamblogs/copying-sealed-trait-instances-a-journey-through-generic-programming-and-shapeless
    def map(f: Int => Int): TimeAmount
  }
  case class Years(n: Int) extends TimeAmount {
    def map(f: Int => Int) = copy(f(n))
  }
  case class Months(n: Int) extends TimeAmount {
    def map(f: Int => Int) = copy(f(n))
  }
  case class Days(n: Int) extends TimeAmount {
    def map(f: Int => Int) = copy(f(n))
  }
  case class Hours(n: Int) extends TimeAmount {
    def map(f: Int => Int) = copy(f(n))
  }
  case class Minutes(n: Int) extends TimeAmount {
    def map(f: Int => Int) = copy(f(n))
  }
  case class Seconds(n: Int) extends TimeAmount {
    def map(f: Int => Int) = copy(f(n))
  }

  implicit class IntTime(n: Int) {
    def years = Years(n)
    def year = Years(n)
    def months = Months(n)
    def month = Months(n)
    def days = Days(n)
    def day = Days(n)
    def hours = Hours(n)
    def hour = Hours(n)
    def minutes = Minutes(n)
    def minute = Minutes(n)
    def seconds = Seconds(n)
    def second = Seconds(n)
  }
}

case class DateRange(start: LocalDateTime, end: LocalDateTime)

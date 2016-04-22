package chee.properties

import scala.util._
import org.scalatest._

class DateTimeTest extends FlatSpec with Matchers {
  import LocalDateTime._

  "LocalDateTime lt,gt" should "return true if year differs" in {
    val n1 = LocalDateTime(1914,3,28,2,55,31)
    val n2 = LocalDateTime(2020,9,21,22,43,31)
    val nx = n1 + n2.year.years

    (n1 < n2) should be (true)
    (n2 > n1) should be (true)
    (n1 < nx) should be (true)
    (n2 < nx) should be (true)
    (nx > n1) should be (true)
    (nx > n2) should be (true)
  }

  "LocalDateTime" should "increment" in {
    val data = List(
      (LocalDateTime(2000, 8, 10, 12, 12, 12), 5, LocalDateTime(2001, 1, 10, 12, 12, 12)),
      (LocalDateTime(2000, 2, 10, 12, 12, 12), 5, LocalDateTime(2000, 7, 10, 12, 12, 12)),
      (LocalDateTime(2000, 8, 10, 12, 12, 12), 12, LocalDateTime(2001, 8, 10, 12, 12, 12)),
      (LocalDateTime(2000, 8, 10, 12, 12, 12), 24, LocalDateTime(2002, 8, 10, 12, 12, 12))
    )
    for ((a, count, exp) <- data) {
      (a + count.months) should be (exp)
    }
  }

  "DateTime lt,gt" should "return true for different values" in {
    val d1 = DateTime.now
    val d2 = DateTime(d1.instant + 1)
    (d1 < d2) should be (true)
    (d2 > d1) should be (true)
  }

  "LocalDateTime.atEnd" should "find correct day of month" in {
    LocalDateTime.atEnd(2015, Some(6)) should be (LocalDateTime(2015,6,30, 23,59,59))
    LocalDateTime.atEnd(2015, Some(2)) should be (LocalDateTime(2015,2,28, 23,59,59))
    LocalDateTime.atEnd(2016, Some(2)) should be (LocalDateTime(2016,2,29, 23,59,59))
  }

  "asString" should "pad with zeros" in {
    LocalDateTime(2000,1,1,1,1,1).asString should be ("2000-01-01 01:01:01")
  }

  "format" should "work with patterns" in {
    val dt = DateTime(1450888707555L)
    dt.format("yyyy") should be ("2015")

    LocalDateTime(2015,10,10,23,10,2).format("yyyy") should be ("2015")
  }


  "DateTimeParser" should "recognize full date/times" in {
    val parser = new DateTimeParser(LocalDateTime.now)
    for {
      d <- List(
        "2010-10-10 12:20:00",
        "2010*10*10*12*20*00",
        "2010/10/10/12/20/0",
        "2010:10:10 12:20:00",
        "2010-10-10T12:20:00"
      )
      atEnd <- List(true, false)
    } parser.parseDate(d, atEnd) should be (Right(LocalDateTime(2010, 10, 10, 12, 20, 0)))
  }

  it should "recognize year/month pairs" in {
    val now = LocalDateTime(2015,10,15,14,32,10)
    val parser = new DateTimeParser(now)
    parser.parseDate("2010-8", true) should be (Right(LocalDateTime.atEnd(2010, Some(8))))
    parser.parseDate("2010-8", false) should be (Right(LocalDateTime.atStart(2010, Some(8))))
  }

  it should "recognize days" in {
    val now = LocalDateTime(2015,10,15,14,32,10)
    val parser = new DateTimeParser(now)
    parser.parseDate("21", true) should be (Right(LocalDateTime.atEnd(2015, Some(9), Some(21))))
    parser.parseDate("3", true) should be (Right(LocalDateTime.atEnd(2015, Some(10), Some(3))))
  }

  it should "parse time amounts" in {
    val now = LocalDateTime(2015,10,15,14,32,10)
    val parser = new DateTimeParser(now)
    val tests = List(
      ("3h", Hours(3)),
      ("8min", Minutes(8)),
      ("4m", Months(4)),
      ("7d", Days(7)),
      ("2y", Years(2)))
    for ((str, amount) <- tests) {
      parser.parseAll(parser.timeAmount, str) match {
        case parser.Success(a, _) =>
          a should be (amount)
        case f => sys.error(f.toString)
      }
    }
  }

  it should "calculate from now" in {
    val now = LocalDateTime(2015,10,15,14,32,10)
    val parser = new DateTimeParser(now)
    parser.parseDate("-4", true) should be (Right(LocalDateTime(2015,10,11,0,0,0)))
    parser.parseDate("-4m", true) should be (Right(LocalDateTime(2015,6,15,0,0,0)))
    parser.parseDate("-4y", true) should be (Right(LocalDateTime(2011,10,15,0,0,0)))
    parser.parseDate("-4d", true) should be (Right(LocalDateTime(2015,10,11,0,0,0)))
    parser.parseDate("-4h", true) should be (Right(LocalDateTime(2015,10,15,10,0,0)))
    parser.parseDate("-4min", true) should be (Right(LocalDateTime(2015,10,15,14,28,0)))
  }

  it should "recognize eastersun keyword" in {
    val now = LocalDateTime(2015,10,15,14,32,10)
    val parser = new DateTimeParser(now)
    parser.parseDate("2014-eastersun", false) should be (Right(atStart(2014, Some(4), Some(20))))
  }

  it should "calculate from fixed dates" in {
    val now = LocalDateTime(2015,10,15,14,32,10)
    val parser = new DateTimeParser(now)
    parser.parseDate("2015-10#-2d", false) should be (Right(LocalDateTime(2015, 9, 29, 0,0,0)))
    parser.parseDate("2014-eastersun#-2d", false) should be (Right(atStart(2014, Some(4), Some(18))))
    parser.parseDate("now#-7d", false) should be (Right(LocalDateTime(2015, 10, 8, 0, 0, 0)))
  }

  "LocalDateTime.eastersun" should "have no typos" in {
    eastersun(2014) should be (atStart(2014, Some(4), Some(20)))
    eastersun(2015) should be (atStart(2015, Some(4), Some(5)))
    eastersun(2016) should be (atStart(2016, Some(3), Some(27)))
  }

}

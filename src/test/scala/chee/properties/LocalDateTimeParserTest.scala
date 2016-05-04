package chee.properties

import fastparse.core.Parsed
import org.scalatest._
import fastparse.core.Parsed.{ Success, Failure }
import LocalDateTime._

class LocalDateTimeParserTest extends FlatSpec with Matchers {

  val now = LocalDateTime(2015,10,15,14,32,10)
  val parserF = (atEnd: Boolean) => new LocalDateTimeParser(now, atEnd)
  val dp = parserF(true)

  "fullSpec" should "parse date strings" in {
    val Success(2016, _) = LocalDateTimeParser.fourDigits.parse("2016")
    val Success(_, _) = LocalDateTimeParser.noDigit.parse("-")
    val Success(_, _) = LocalDateTimeParser.noDigit.parse(":")
    val Success(_, _) = LocalDateTimeParser.noDigit.parse(" ")

    val dates = List(
      "2016-10-15 12:12:15",
      "2016/10/15T12-12-15",
      "2016-10-15T12-12-15",
      "2016,10,15,12,12,15",
      "2016.10.15T12.12.15",
      "2016 10 15 12 12 15"
    )
    for (date <- dates) {
      dp.fullDateTime.parse(date) match {
        case Success(LocalDateTime(2016, 10, 15, 12, 12, 15), _) =>
        case Success(date, _) => sys.error(s"unexpected result: $date")
        case f: Failure => sys.error(f.msg)
      }
    }
  }

  it should "fail on wrong input" in {
    val Failure(_, 2, _) = LocalDateTimeParser.fourDigits.parse("15")
    val Failure(_, 0, _) = LocalDateTimeParser.fourDigits.parse("a201")
    val Failure(_, 3, _) = LocalDateTimeParser.fourDigits.parse("200b")
    val Failure(_, 3, _) = LocalDateTimeParser.fourDigits.parse("201")
  }


  "parseDate" should "recognize full date/times" in {
    for {
      d <- List(
        "2010-10-10 12:20:00",
        "2010*10*10*12*20*00",
        "2010/10/10/12/20/0",
        "2010:10:10 12:20:00",
        "2010-10-10T12:20:00"
      )
      atEnd <- List(true, false)
    } parserF(atEnd).parseDate(d) should be (Right(LocalDateTime(2010, 10, 10, 12, 20, 0)))
  }

  it should "recognize year/month pairs" in {
    parserF(true).parseDate("2010-8") should be (Right(LocalDateTime.atEnd(2010, Some(8))))
    parserF(false).parseDate("2010-8") should be (Right(LocalDateTime.atStart(2010, Some(8))))
  }

  it should "recognize days" in {
    dp.parseDate("21") should be (Right(LocalDateTime.atEnd(2015, Some(9), Some(21))))
    dp.parseDate("3") should be (Right(LocalDateTime.atEnd(2015, Some(10), Some(3))))
  }

  it should "calculate from now" in {
    dp.parseDate("-4") should be (Right(LocalDateTime(2015,10,11,0,0,0)))
    dp.parseDate("-4m") should be (Right(LocalDateTime(2015,6,15,0,0,0)))
    dp.parseDate("-4y") should be (Right(LocalDateTime(2011,10,15,0,0,0)))
    dp.parseDate("-4d") should be (Right(LocalDateTime(2015,10,11,0,0,0)))
    dp.parseDate("-4h") should be (Right(LocalDateTime(2015,10,15,10,0,0)))
    dp.parseDate("-4min") should be (Right(LocalDateTime(2015,10,15,14,28,0)))
  }

  it should "recognize eastersun keyword" in {
    val parser = parserF(false)
    parser.parseDate("2014-eastersun") should be (Right(atStart(2014, Some(4), Some(20))))
  }

  it should "calculate from fixed dates" in {
    val parser = parserF(false)
    parser.parseDate("2015-10#-2d") should be (Right(LocalDateTime(2015, 9, 29, 0,0,0)))
    parser.parseDate("2014-eastersun#-2d") should be (Right(atStart(2014, Some(4), Some(18))))
    parser.parseDate("now#-7d") should be (Right(LocalDateTime(2015, 10, 8, 0, 0, 0)))
  }

  "timeAmount" should "parse time amounts" in {
    val tests = List(
      ("3h", Hours(3)),
      ("8min", Minutes(8)),
      ("4m", Months(4)),
      ("7d", Days(7)),
      ("2y", Years(2)))
    for ((str, amount) <- tests) {
      LocalDateTimeParser.timeAmount.parse(str) match {
        case Parsed.Success(a, _) =>
          a should be (amount)
        case f => sys.error(f.toString)
      }
    }
  }

}

package chee.properties

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
      (LocalDateTime(2000, 8, 10, 12, 12, 12), 0, LocalDateTime(2000, 8, 10, 12, 12, 12)),
      (LocalDateTime(2000, 8, 10, 12, 12, 12), -3, LocalDateTime(2000, 5, 10, 12, 12, 12)),
      (LocalDateTime(2000, 8, 10, 12, 12, 12), -8, LocalDateTime(1999, 12, 10, 12, 12, 12))
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

  "eastersun" should "have no typos" in {
    eastersun(2014) should be (atStart(2014, Some(4), Some(20)))
    eastersun(2015) should be (atStart(2015, Some(4), Some(5)))
    eastersun(2016) should be (atStart(2016, Some(3), Some(27)))
  }
}

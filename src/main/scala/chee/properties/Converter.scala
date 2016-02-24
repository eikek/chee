package chee.properties

import scala.util.Try

trait Converter[V] {
  def parse(str: String): Either[String, V]
  def render(v: V): String = v.toString
}

object LongConverter extends Converter[Long] {
  def parse(str: String) =
    Try(java.lang.Long.parseLong(str)).toEither("Invalid number: ")
}

object IntConverter extends Converter[Int] {
  def parse(str: String) =
    Try(java.lang.Integer.parseInt(str)).toEither("Invalid number: ")
}

object MegaPixelConverter extends Converter[Int] {
  val megaPixel = new UnitConverter(List(
    "mp" -> (1000 * 1000),
    "kp" -> 1000), "p", "Invalid pixel")

  def parse(str: String) =
    megaPixel.parse(str).right.map(_.toInt)

  override def render(p: Int) = megaPixel.render(p)
}

object FileSizeConverter extends Converter[Long] {
  val fileSize = new UnitConverter(List(
    "gb" -> (1024 * 1024 * 1024),
    "mb" -> (1024 * 1024),
    "kb" -> 1024),
    "b",
    "Invalid size")

  def parse(str: String) = fileSize.parse(str)
  override def render(p: Long) = fileSize.render(p)
}

object LocalDateTimeConverter extends Converter[LocalDateTime] {
  def parse(str: String) =
    new DateTimeParser(LocalDateTime.now).parseDate(str, false)

  override def render(ld: LocalDateTime) = ld.asString
}

object DateTimeConverter extends Converter[Long] {
  override def parse(str: String) = {
    LongConverter.parse(str) match {
      case Right(l) => Right(l)
      case _ =>
        //todo also try some iso/standard patterns
        //todo timezone!
        LocalDateTimeConverter.parse(str).right.map(_.toDateTime().instant)
    }
  }
  override def render(dt: Long) = DateTime(dt).format("yyyy-MM-dd HH:mm:ss")
}

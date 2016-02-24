package chee.properties

import scala.math.Ordering

trait Value[V] extends Ordering[V] with Converter[V] {
  def compare(a: V, b: V): Int
  def like(v: V, glob: String): Either[String, Boolean] =
    Value.like(render(v), glob)

  def evalConv(a: V, comp: Comp, b: V): Either[String, Boolean] = comp match {
    case Comp.Lt => Right(lt(a, b))
    case Comp.Gt => Right(gt(a, b))
    case Comp.Eq => Right(a == b)
    case _ => Left(s"Unknown comparator: $comp")
  }
  def eval(mapValue: V, comp: Comp, search: String): Either[String, Boolean] = {
    comp match {
      case Comp.Like => like(mapValue, search)
      case _ => for {
        b <- parse(search).right
        r <- evalConv(mapValue, comp, b).right
      } yield r
    }
  }
  def evalString(mapValue: String, comp: Comp, search: String): Either[String, Boolean] =
    for {
      mapval <- parse(mapValue).right
      r <- eval(mapval, comp, search).right
    } yield r

  def humanReadable(raw: String): Either[String, String] =
    for (v <- parse(raw).right) yield render(v)
}

object Value {

  def like(value: String, glob: String): Either[String, Boolean] = {
    import java.util.regex.Pattern
    import scala.util.{Try, Success, Failure}

    def quote(s: String) =
      if (s.isEmpty) ""
      else Pattern.quote(s.replaceAll("\n", """\n"""))

    if (glob.exists(java.lang.Character.isISOControl)) Left("Control characters not allowed in filter expression.")
    else if (glob == "*") Right(true)
    else if (glob.indexOf('*') < 0) Right(glob.equalsIgnoreCase(value)) // includes case where expression is empty
    else Try(Pattern.compile(glob.split("\\*", -1).map(quote).mkString(".*"), Pattern.CASE_INSENSITIVE)) match {
      case Success(p) => Right(p.matcher(value).matches)
      case Failure(ex) => Left(ex.getMessage)
    }
  }

  def forIdent(ident: Ident): Value[_] = ident match {
    case EndsWith(Ident.length) => FileSizeValue
    case EndsWith(Ident.lastModified) => DateTimeValue
    case EndsWith(Ident.added) => DateTimeValue
    case EndsWith(Ident.created) => LocalDateTimeValue
    case EndsWith(Ident.width) => IntValue
    case EndsWith(Ident.height) => IntValue
    case EndsWith(Ident.iso) => IntValue
    case EndsWith(Ident.orientation) => IntValue
    case EndsWith(VirtualProperty.idents.pixel) => MegapixelValue
    case _ => StringValue
  }

  private object EndsWith {
    def unapply(id: Ident): Option[Ident] = {
      val idx = id.name.lastIndexOf('-')
      if (idx > 0) Some(Ident(id.name.substring(idx+1)))
      else Some(id)
    }
  }
}

object StringValue extends Value[String] {
  def compare(a: String, b: String) = a compareTo b
  def parse(str: String) = Right(str)
  override def render(str: String) = str
}

abstract class ConvValue[V](conv: Converter[V]) extends Value[V] {
  override def parse(str: String) = conv.parse(str)
  override def render(v: V) = conv.render(v)
}

trait IntCompare {
  def compare(a: Int, b: Int) = a compareTo b
}

trait LongCompare {
  def compare(a: Long, b: Long) = a compareTo b
}

object LongValue extends ConvValue[Long](LongConverter) with LongCompare
object IntValue extends ConvValue[Int](IntConverter) with IntCompare
object MegapixelValue extends ConvValue[Int](MegaPixelConverter) with IntCompare
object FileSizeValue extends ConvValue[Long](FileSizeConverter) with LongCompare
object LocalDateTimeValue extends ConvValue[LocalDateTime](LocalDateTimeConverter) {
  def compare(a: LocalDateTime, b: LocalDateTime) =
    if (a < b) -1
    else if (a == b) 0
    else +1
}
object DateTimeValue extends ConvValue[Long](DateTimeConverter) with LongCompare

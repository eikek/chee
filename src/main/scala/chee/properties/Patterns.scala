package chee.properties

object Patterns {
  import MapGet._

  type Pattern = MapGet[Either[String, String]]
  type PatternPred = MapGet[Either[String, Boolean]]

  implicit class MapGetEitherOps[A,B](p: MapGet[Either[A,B]]) {
    def rmap[C](f: B => C): MapGet[Either[A, C]] =
      p.map(_.right.map(f))

    def rmap2[C](f: B => Either[A, C]): MapGet[Either[A, C]] =
      p.map(_.right.flatMap(f))

    def rflatMap[C](f: B => MapGet[C]): MapGet[Either[A, C]] =
      p.flatMap {
        case Right(b) => f(b).map(Right(_))
        case Left(err) => unit(Left(err))
      }

    def rflatMap2[C](f: B => MapGet[Either[A, C]]): MapGet[Either[A, C]] =
      p.flatMap {
        case Right(b) => f(b)
        case Left(e) => unit(Left(e))
      }

    def right(implicit err: A => Nothing = a => sys.error(a.toString)): MapGet[B] = p.map {
      case Right(s) => s
      case Left(e) => err(e)
    }
  }

  def raw(s: String): Pattern = unit(Right(s))

  val empty = raw("")

  val newline = raw("\n")

  def findIdent(id: Ident): MapGet[Either[String, Ident]] =
    idents(true).map { ids =>
      Ident.findIdent(ids.toSet + Ident("ident"), id)
    }

  def readable(ident: Ident, searchIdent: Boolean = true): Pattern = {
    val convert: Option[Property] => Either[String, String] = {
      case None => Right("")
      case Some(Property(id, raw)) =>
        Value.forIdent(id).humanReadable(raw)
    }
    if (searchIdent) findIdent(ident).rflatMap(find).rmap2(convert)
    else find(ident).map(convert)
  }

  def lookup(ident: Ident, format: Option[String] = None, searchIdent: Boolean = true): Pattern = {
    import scala.util.Try
    def convert(prop: Option[Property]): Either[String, String] = (format, prop) match {
      case (_, None) => Right("")
      case (None, Some(Property(id, v))) => Right(v)
      case (Some(f), Some(Property(id, v))) =>
        Value.forIdent(id) match {
          case LocalDateTimeValue =>
            LocalDateTimeConverter.parse(v).right.map(_.format(f))
          case DateTimeValue =>
            Try(DateTime(v.toLong).format(f)).toEither(s"Cannot format datetime `$v' using `$f': ")
          case _: IntCompare => Try(f.format(v.toInt)).toEither(s"Cannot format number `$v' using `$f': ")
          case _: LongCompare => Try(f.format(v.toLong)).toEither(s"Cannot format number `$v' using `$f': ")
          case _ => Try(f.format(v)).toEither(s"Cannot format `$v' using `$f': ")
        }
    }

    if (searchIdent) findIdent(ident).rflatMap(find).rmap2(convert)
    else find(ident).map(convert)
  }

  def existsIdent(id: Ident, searchIdent: Boolean = true): PatternPred =
    if (searchIdent) findIdent(id).rflatMap(Predicates.exists)
    else Predicates.exists(id).map(Right(_))

  def quote(quoteChar: Character, p: Pattern): Pattern = p.rmap { str =>
    if (str matches "[0-9]+") str
    else s"""${quoteChar}${str.replace(quoteChar.toString, "\\"+quoteChar)}${quoteChar}"""
  }

  def fixedwidth(len: Int, p: Pattern, frontPad: Boolean = true, leftLen: Int = 8, ellipsis: String = "…"): Pattern =
    p.rmap { s =>
      val slen = s.length
      if (len == slen) s
      else if (slen < len) {
        val pad = List.fill(len - slen)(" ")
        if (frontPad) pad.mkString + s
        else s + pad.mkString
      } else {
        if (len < (leftLen * 2 + 1)) s.substring(0, len - ellipsis.length) + ellipsis
        else s.substring(0, leftLen) + ellipsis + s.substring(s.length - len + leftLen + ellipsis.length)
      }
    }

  def maxlen(len: Int, p: Pattern, left: Boolean = true): Pattern =
    p.rmap { s =>
      val slen = s.length
      if (slen <= len) s
      else {
        if (left) s.substring(0, len)
        else s.substring(slen-len)
      }
    }

  def cond(c: PatternPred, ifTrue: Pattern, ifFalse: Pattern): Pattern =
    c.rflatMap2 { bool =>
      if (bool) ifTrue else ifFalse
    }

  /** Lookup the first ident from `idents` that exists. */
  def lookupAlt(idents: Seq[Ident], format: Option[String] = None): Pattern =
    idents.foldRight(empty) { (id, pat) =>
      cond(existsIdent(id), lookup(id, format), pat)
    }

  def seqq(ps: Seq[Pattern]): Pattern =
    MapGet.joinEitherBiased(ps).map {
      case Right(xs) => Right(xs.mkString)
      case Left(err) => Left(err)
    }

  def seq(ps: Pattern*): Pattern = seqq(ps)

  def loop(main: Ident => Pattern, stop: Ident => Pattern, idents: MapGet[Seq[Ident]], includeEmpty: Boolean = true): Pattern = {
    type Result = Either[String, String]
    def valueFor(id: Ident) =
      value(id).modify(_.add(Ident("ident") -> id.name)).flatMap { v =>
        if (v.isEmpty && !includeEmpty) pair(empty, empty)
        else pair(main(id), stop(id))
      }.modify(_.remove(Ident("ident")))

    def concat(xs: Seq[(Result, Result)]): Result =
      flatten2(xs) match {
        case Right(body) =>
          Right(body.foldLeft("") {
            case (str, (before, after)) =>
              if (str.isEmpty) before + str
              else before + after + str
          })
        case Left(errs) => Left(errs)
      }

    for {
      ids <- idents
      values <- MapGet.seq(ids.reverse.map(valueFor))
    } yield concat(values)
  }
}

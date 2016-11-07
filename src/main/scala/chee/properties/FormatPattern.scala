package chee.properties

import chee.metadata.idents

object FormatPatterns {
  import Patterns._

  val lisp: Pattern = seq(
    raw("("),
    loop(
      id => seq(raw(":"), lookup('ident), raw(" "), cond(existsIdent(id, false), quote('"', lookup(id)), raw("nil"))),
      id => raw(" "),
      MapGet.idents(false),
      includeEmpty = true),
    raw(")"),
    newline
  )

  private def detail(inclVirt: Boolean): Pattern = seq(
    loop(
      id => seq(fixedwidth(20, seq(readable('ident), raw(":")), frontPad = false), raw(" "), readable(id), newline),
      id => empty,
      MapGet.idents(inclVirt),
      includeEmpty = true),
    newline
  )

  val fullDetail = detail(true)
  val detailNoVirtual = detail(false)

  val date = fixedwidth(17,
    cond(existsIdent(Ident.created, false),
      lookup(Ident.created, Some("yyyy-MM-dd HH:mm")),
      lookup(Ident.lastModified, Some("yyyy-MM-dd HH:mm"))),
    frontPad = false)

  val dimensions = seq(
    fixedwidth(4, lookup(Ident.width), frontPad = true),
    raw("x"),
    fixedwidth(4, lookup(Ident.height), frontPad = false),
    raw(" "),
    fixedwidth(7, readable(VirtualProperty.idents.pixel), frontPad = false)
  )

  val oneline: Pattern = seq(
    date,
    raw(" "),
    dimensions,
    raw(" ("),
    fixedwidth(16, seq(readable(Ident.model), raw(")")), frontPad = false),
    raw(" "),
    fixedwidth(8, readable(Ident.length)),
    raw(" "),
    fixedwidth(30, readable(Ident.location)),
    raw(" "),
    cond(existsIdent(VirtualProperty.idents.encrypted, false), raw("🔒 "), empty),
    readable(Ident.filename),
    newline)

  val onelineNoLocation: Pattern = seq(
    date,
    raw(" "),
    dimensions,
    raw(" ("),
    fixedwidth(16, seq(readable(Ident.model), raw(")")), frontPad = false),
    raw(" "),
    fixedwidth(8, readable(Ident.length)),
    raw(" "),
    readable(Ident.filename),
    newline)

  val paths = seq(lookup(Ident.path), newline)

  val onelineTags = seq(
    readable(idents.tag),
    raw(": "),
    readable(idents.count),
    newline)

  val formats = Map(
    "oneline" -> oneline,
    "oneline-no-location" -> onelineNoLocation,
    "oneline-tags" -> onelineTags,
    "detail" -> fullDetail,
    "detail-no-virtual" -> detailNoVirtual,
    "lisp" -> lisp,
    "paths" -> paths
  )

  private val parser = new PatternParser(Ident.defaults)

  def parse(str: String): Either[String, Pattern] =
    parser.parsePattern(str)
}

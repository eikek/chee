package chee.properties

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

  val detail: Pattern = seq(
    loop(
      id => seq(fixedwidth(20, seq(readable('ident), raw(":")), frontPad = false), raw(" "), readable(id), newline),
      id => empty,
      MapGet.idents(true),
      includeEmpty = true),
    newline
  )

  val oneline: Pattern = seq(
    fixedwidth(17,
      cond(existsIdent(Ident.created, false),
        lookup(Ident.created, Some("yyyy-MM-dd HH:mm")),
        lookup(Ident.lastModified, Some("yyyy-MM-dd HH:mm"))),
      frontPad = false),
    raw(" "),
    seq(
      fixedwidth(4, lookup(Ident.width), frontPad = true),
      raw("x"),
      fixedwidth(4, lookup(Ident.height), frontPad = false),
      raw(" "),
      fixedwidth(7, readable(VirtualProperty.idents.pixel), frontPad = false)
    ),
    raw(" ("),
    fixedwidth(16, seq(readable(Ident.model), raw(")")), frontPad = false),
    raw(" "),
    fixedwidth(8, readable(Ident.length)),
    raw(" "),
    fixedwidth(30, readable(Ident.location)),
    raw(" "),
    readable(Ident.filename),
    newline)


  val paths = seq(lookup(Ident.path), newline)

  private val parser = new PatternParser(Ident.defaults)

  def parse(str: String): Either[String, Pattern] =
    parser.parsePattern(str)
}

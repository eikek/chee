package chee.query

import chee.crypto.CheeCrypt
import chee.util.Render
import chee.properties._
import Condition.Render._

object SqlFormat {

  private val pbc = CheeCrypt.passwordEncryptExtension
  private val pkc = CheeCrypt.publicKeyEncryptExtension

  private val encryptedExpr =
    s"(case when substr(${Ident.path.name}, -4) = '.$pbc' then '$pbc' when substr(${Ident.path.name}, -4) = '.$pkc' then '$pkc' else null end)"

  implicit val _trueCondition: Render[TrueCondition.type] =
    Render(_ => "1=1")

  implicit val _existsCondition: Render[Exists] = Render {
    case Exists(id) => id match {
      case VirtualProperty.idents.pixel =>
        s"(${Ident.width.name} is not null and ${Ident.height.name} is not null)"
      case VirtualProperty.idents.encrypted =>
        s"$encryptedExpr is not null"
      case _ =>
        s"${id.name} is not null"
    }
  }

  private def sqlValue(p: Prop) = {
    def quote(s: String) = s"'$s'"

    def parse(v: Value[_], s: String, comp: Comp, id: Ident): String = v.parse(s) match {
      case Right(x) => x.toString
      case Left(m) => chee.UserError(s"Cannot use `$s' to compare via `${comp.name}' a ${id.name} value.")
    }

    def localDateTimeValue(s: String) =
      LocalDateTimeValue.parse(s) match {
        case Right(dt) => s"datetime(${dt.toDateTime().instant / 1000}, 'unixepoch')"
        case Left(m) => chee.UserError(s"Cannot create a date/time value from `$s': $m")
      }

    if (p.comp == Comp.Like) quote(p.prop.value.replace("*", "%"))
    else Value.forIdent(p.prop.ident) match {
      case c: IntCompare => parse(c, p.prop.value, p.comp, p.prop.ident)
      case c: LongCompare => parse(c, p.prop.value, p.comp, p.prop.ident)
      case LocalDateTimeValue => localDateTimeValue(p.prop.value)
      case _ => quote(p.prop.value)
    }
  }

  private def columnSql(id: Ident, comp: Comp): String = id match {
    case VirtualProperty.idents.pixel =>
      s"(${Ident.width.name} * ${Ident.height.name})"
    case VirtualProperty.idents.encrypted =>
      encryptedExpr
    case Ident.added if comp == Comp.Like => "datetime(added/1000, 'unixepoch')"
    case Ident.lastModified if comp == Comp.Like => "datetime(lastmodified/1000, 'unixepoch')"
    case Ident.created if comp != Comp.Like => "datetime(created)"
    case id => id.name
  }

  private def operatorSql(comp: Comp) = comp match {
    case Comp.Like => "like"
    case c => c.name
  }

  implicit val _inCondition: Render[In] = Render {
    case In(id, values) =>
      val v = values.map(v => sqlValue(Prop(Comp.Eq, id -> v.toLowerCase))).mkString("(", ", ", ")")
      s"lower(${id.name}) in $v"
  }

  implicit val _propCondition: Render[Prop] = Render {
    case p@Prop(comp, Property(id, value)) =>
      s"${columnSql(id, comp)} ${operatorSql(comp)} ${sqlValue(p)}"
  }

  implicit val _identPropRender: Render[IdentProp] = Render {
    case IdentProp(comp, id1, id2) =>
      s"${columnSql(id1, comp)} ${operatorSql(comp)} ${columnSql(id2, comp)} "
  }

  implicit def _juncRender: Render[Junc] = Render {
    case Junc(op, nodes) =>
      if (nodes.isEmpty) ""
      else {
        val r = implicitly[Render[Condition]]
        val comb = if (op == Junc.Or) " or " else " and "
        nodes.map(r.render).mkString("(", comb, ")")
      }
  }

  implicit def _notRender: Render[Not] = Render {
    case Not(c) =>
      val r = implicitly[Render[Condition]]
      s"not(${r.render(c)})"
  }

  def render(c: Condition): String = {
    val r = implicitly[Render[Condition]]
    r.render(c)
  }
}

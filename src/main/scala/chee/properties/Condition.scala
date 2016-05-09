package chee.properties

import chee.util.Render

sealed trait Condition
sealed trait Leaf extends Condition
case class Prop(comp: Comp, prop: Property) extends Condition with Leaf {
  val ident = prop.ident
  val value = prop.value
}
case class IdentProp(comp: Comp, id1: Ident, id2: Ident) extends Condition with Leaf
case object TrueCondition extends Condition with Leaf
case class Exists(ident: Ident) extends Condition with Leaf
case class Junc(op: Junc.Op, nodes: List[Condition]) extends Condition
case class Not(cond: Condition) extends Condition
case class In(id: Ident, values: Seq[String]) extends Condition with Leaf

object Junc {
  sealed trait Op
  case object Or extends Op
  case object And extends Op
}

object Prop {
  def apply(name: Ident, comp: Comp, value: String): Prop =
    Prop(comp, name -> value)
}

case class Comp(name: String)

object Comp {
  val Eq = Comp("=")
  val Like = Comp(":")
  val Lt = Comp("<")
  val Gt = Comp(">")

  val all = Set(Eq, Lt, Gt, Like)

  def isDefault(c: Comp) = all.contains(c)
}

object Condition {
  def and(nodes: Condition*) = Junc(Junc.And, nodes.toList)
  def or(nodes: Condition*) = Junc(Junc.Or, nodes.toList)
  def not(node: Condition) = Not(node)

  def eq(p: Property) = Prop(Comp.Eq, p)
  def like(p: Property) = Prop(Comp.Like, p)
  def lt(p: Property) = Prop(Comp.Lt, p)
  def gt(p: Property) = Prop(Comp.Gt, p)

  def mapAll(f: Condition => Condition)(c: Condition): Condition = c match {
    case l: Leaf => f(l)
    case Junc(op, nodes) => f(Junc(op, nodes.map(mapAll(f))))
    case Not(node) => f(Not(mapAll(f)(node)))
  }

  /** Remove empty junctions and double negation. */
  val normalize: Condition => Condition = mapAll {
    case Junc(op, nodes) =>
      val next = nodes.filter {
        case Junc(op, Nil) => false
        case _ => true
      }
      next match {
        case a :: Nil => a
        case _ => Junc(op, next)
      }
    case Not(Not(x)) => x
    case n => n
  }

  object Render {
    implicit def _conditionRender(implicit
      r0: Render[TrueCondition.type],
      r1: Render[Exists],
      r2: Render[Prop],
      r3: Render[IdentProp],
      r4: Render[Not],
      r5: Render[Junc],
      r6: Render[In]): Render[Condition] =
      chee.util.Render {
        case c@TrueCondition => r0.render(c)
        case c: Exists => r1.render(c)
        case c: Prop => r2.render(c)
        case c: IdentProp => r3.render(c)
        case c: Not => r4.render(c)
        case c: Junc => r5.render(c)
        case c: In => r6.render(c)
      }
  }
}

object ConditionFormat {
  import Condition.Render._

  private def makeValue(escape: Seq[Char])(s: String) = {
    val str = escape.foldLeft(s) { (str, c) =>
      str.replace(s"$c", s"\\$c")
    }
    s"${str}"
  }

  implicit val _trueCondition: Render[TrueCondition.type] =
    Render(_ => "true")
  implicit val _existsCondition: Render[Exists] = Render {
    case Exists(id) => s"${id.name}?"
  }
  implicit val _propCondition: Render[Prop] = Render {
    case Prop(comp, Property(id, value)) =>
      s"""${id.name}${comp.name}'${makeValue("'")(value)}'"""
  }
  implicit val _identPropRender: Render[IdentProp] = Render {
    case IdentProp(comp, id1, id2) => s"${id1.name}${comp.name}'${id2.name}"
  }
  implicit val _inRender: Render[In] = Render {
    case In(id, values) => s"""${id.name}~'${values.map(makeValue("';")).mkString(";")}'"""
  }

  implicit def _juncRender: Render[Junc] = Render {
    case Junc(op, nodes) =>
      if (nodes.isEmpty) ""
      else {
        val r = implicitly[Render[Condition]]
        s"""(${if (op == Junc.Or) "|" else "&"} ${nodes.map(n => r.render(n)).mkString(" ")})"""
      }
  }
  implicit def _notRender: Render[Not] = Render {
    case Not(c) =>
      val r = implicitly[Render[Condition]]
      s"!${r.render(c)}"
  }

  def render(c: Condition): String = {
    val r = implicitly[Render[Condition]]
    r.render(c)
  }
}

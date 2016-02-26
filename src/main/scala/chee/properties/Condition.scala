package chee.properties

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

  def reduce[B](
    leaf: Leaf => B,
    join: Junc.Op => (B, B) => B,
    junc: Junc.Op => Option[B] => B,
    neg: B => B)(c: Condition): B =
    c match {
      case e: Leaf => leaf(e)
      case Junc(op, nodes) => junc(op) {
        Option(nodes).filter(_.nonEmpty) map { ns =>
          ns.map(reduce(leaf, join, junc, neg)).reduce(join(op))
        }
      }
      case Not(node) => neg(reduce(leaf, join, junc, neg)(node))
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

  def render(c: Condition) = Condition.reduce[String](
    leaf => leaf match {
      case Exists(id) => s"${id.name}?"
      case Prop(comp, Property(id, value)) => s"""${id.name}${comp.name}'${value.replaceAll("'", "\'")}'"""
      case IdentProp(comp, id1, id2) => s"${id1.name}${comp.name}'${id2.name}"
      case TrueCondition => "true"
    },
    op => (s1, s2) => s1 +" "+ s2,
    op => os => s"""(${if (op == Junc.Or) "|" else "&"} """ + os.getOrElse("") + ")",
    s => s"!$s")(c)

}

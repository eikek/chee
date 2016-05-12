package chee.metadata

import chee.UserError
import chee.query.Transform
import chee.properties._
import MetadataMacro._
import com.typesafe.scalalogging.LazyLogging

class MetadataMacro(f: Condition => Traversable[String]) extends Transform with LazyLogging {
  def this(mf: MetadataFile) = this(mf.findIds _)

  def mapCondition(c: Condition): Condition = {
    logger.trace(s"Search metadata for $c")
    val ids = f(c)
    logger.debug(s"Found ${ids.size} metadata results for condition ${c}")
    if (ids.isEmpty) Not(TrueCondition)
    else In(Ident.checksum, ids.toSeq)
  }

  def apply(c: Condition): Condition = Condition.mapAll ({
    case p@Prop(_, Property(id, _)) if metadataId(id) =>
      mapCondition(p)
    case IdentProp(comp, id1, id2) if metadataId(id1, id2) =>
      UserError(s"Cannot compare ${id1.name} with ${id2.name} (not supported)")
    case e@Exists(id) if metadataId(id) =>
      mapCondition(e)
    case c@In(id, _) if metadataId(id) =>
      mapCondition(c)
    case n => n
  })(c)

}

object MetadataMacro {
  val metaIdents = idents.all.toSet

  def metadataId(id: Ident*): Boolean =
    id.exists(metaIdents)
}

package chee.metadata

import better.files._
import chee.properties._
import MapGet._
import chee.metadata.{ idents => midents }

class MetadataExtract(mf: MetadataFile, mapping: Ident => Ident = identity) extends Extraction {

  val idents: Set[Ident] = midents.all.toSet.map(mapping)

  def mapIdents(f: Ident => Ident) =
    new MetadataExtract(mf, f)

  def extractM(file: File): MapGet[PropertyMap] =
    value(mapping(Ident.checksum)).map {
      case Some(id) =>
        mf.find(Prop(Comp.Eq, Ident.checksum -> id))
          .headOption
          .map(mapget.makePropertyMap(mapping).result)
          .getOrElse(PropertyMap.empty)
      case _ => PropertyMap.empty
    }
}

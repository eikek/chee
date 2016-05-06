package chee

import chee.properties._
import chee.metadata.RecElement._
import MapGet._

package metadata {

  case class Tag(name: String)

  object Tag {

    private [chee] val separator = "|"

    val tagRegex = (s"[^\\s,${separator}]+").r
  }

  object idents {
    val tag: Ident = 'tag
    val comment: Ident = 'comment

    val all = List(tag, comment)
  }

  object mapget {

    val tagValues: MapGet[Seq[Tag]] =
      value(idents.tag).map {
        case Some(s) if s startsWith "[" =>
          s.substring(1, s.length-1)
            .split(Tag.separator.charAt(0))
            .map(n => Tag(n.trim)).toSeq
        case Some(s) =>
          s.split(",").map(n => Tag(n.trim)).toSeq
        case None => Seq.empty
      }

    def setTags(tags: Seq[Tag]): MapGet[Unit] = modify { m =>
      m + (idents.tag -> tags.map(_.name).mkString(Tag.separator, Tag.separator, Tag.separator))
    }

    def setTags(t1: Tag, ts: Tag*): MapGet[Unit] =
      setTags(t1 +: ts)

    def setComment(c: String): MapGet[Unit] = modify { m =>
      if (c.isEmpty) m
      else m + (idents.comment -> c)
    }

    def setId(id: String): MapGet[Unit] = modify { m =>
      m + (Ident.checksum -> id)
    }

    val makeRecord: MapGet[Record] =
      pair(valueForce(Ident.checksum), pair(tagValues, value(idents.comment))).map {
        case (id, (tags, comment)) =>
          Record()
            .set("Checksum", id)
            .set("Tag", tags.map(_.name): _*)
            .set("Comment", comment.toSeq: _*)
      }

    val idAndRecord: MapGet[(String, Record)] =
      pair(valueForce(Ident.checksum), makeRecord)
  }
}

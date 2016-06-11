package chee

import chee.properties._
import chee.metadata.RecElement._
import MapGet._

package metadata {

  case class Tag(name: String)

  object Tag {
    private [chee] val separator = "|"

    val tagRegex = s"[^\\s,;${separator}]+"

    def validated(name: String): Tag = {
      if (name matches tagRegex) Tag(name)
      else UserError(s"`$name' is not a valid tag name! Must match `$tagRegex'.")
    }
  }

  object idents {
    val tag: Ident = 'tag
    val comment: Ident = 'comment

    val all = List(tag, comment)
  }

  object mapget {

    val tagValues: MapGet[Seq[Tag]] =
      value(idents.tag).map {
        case Some(s) if s startsWith Tag.separator =>
          s.substring(1, s.length-1)
            .split(Tag.separator.charAt(0))
            .map(n => Tag(n.trim)).toSeq
        case Some(s) =>
          s.split(",").map(n => Tag(n.trim)).toSeq
        case None => Seq.empty
      }

    def setTags(tags: Seq[Tag]): MapGet[Unit] = modify { m =>
      if (tags.isEmpty) m.remove(idents.tag)
      else m + (idents.tag -> tagsToTagString(tags))
    }

    def setTags(t1: Tag, ts: Tag*): MapGet[Unit] =
      setTags(t1 +: ts)

    def addTags(tags: Seq[Tag]): MapGet[Unit] =
      tagValues.flatMap { old =>
        modify { m =>
          m + (idents.tag -> tagsToTagString((old ++ tags).distinct))
        }
      }

    def addTags(t1: Tag, ts: Tag*): MapGet[Unit] = addTags(t1 +: ts)

    def removeTags(tags: Seq[Tag]): MapGet[Unit] =
      tagValues.flatMap { old =>
        modify { m =>
          m + (idents.tag -> tagsToTagString(old diff tags))
        }
      }

    def removeTags(t1: Tag, ts: Tag*): MapGet[Unit] = removeTags(t1 +: ts)

    def removeAllTags = setTags(Seq.empty)

    def setComment(c: String): MapGet[Unit] = modify { m =>
      if (c.isEmpty) m.remove(idents.comment)
      else m + (idents.comment -> c)
    }

    def setId(id: String): MapGet[Unit] = modify { m =>
      m + (Ident.checksum -> id)
    }

    val makeRecord: MapGet[Record] =
      pair(valueForce(Ident.checksum), pair(value(idents.comment), tagValues)).map {
        case (id, (comment, tags)) =>
          Record()
            .set(Ident.checksum.name.capitalize, id)
            .set(idents.comment.name.capitalize, comment.toSeq: _*)
            .set(idents.tag.name.capitalize, tags.map(_.name): _*)
      }

    def makePropertyMap(mapping: Ident => Ident): MapGet[PropertyMap] =
      pair(valueForce(Ident.checksum), pair(value(idents.tag), value(idents.comment))).map {
        case (id, (tags, comment)) =>
          PropertyMap.empty +
            (mapping(Ident.checksum) -> id) +?
            tags.map(mapping(idents.tag) -> _) +?
            comment.map(mapping(idents.comment) -> _)
      }

    val idAndRecord: MapGet[(String, Record)] =
      pair(valueForce(Ident.checksum), makeRecord)
  }
}

package object metadata {

  private[metadata] def fieldsToTagString(fs: Seq[RecElement.Field]): String =
    stringsToTagString(fs.map(_.value))

  private[metadata] def tagsToTagString(ts: Seq[Tag]): String =
    stringsToTagString(ts.map(_.name))

  @inline
  private def stringsToTagString(s: Seq[String]): String =
    s.sorted.mkString(Tag.separator, Tag.separator, Tag.separator)
}

package chee

import scala.util.Try
import chee.properties._
import chee.properties.MapGet._
import com.sksamuel.scrimage._
import com.sksamuel.scrimage.nio._
import better.files._

object Processing {
  private val image: MapGet[Option[Image]] = existingPath.map {
    case Some(f) => Try(Image.fromPath(f.path)).toOption
    case _ => None
  }

  val targetWidth = Ident.width.in("target")
  val targetHeight = Ident.height.in("target")

  private def setTargetSize(size: Size): MapGet[Unit] =
    modify(m => m +
      (targetWidth -> size.width.toString) +
      (targetHeight -> size.height.toString))

  private val removeTargetSize =
    modify(lm => lm remove targetWidth remove targetHeight)


  private def makeOut(size: Size, outFile: MapGet[File]): MapGet[File] =
    for {
      _ <- setTargetSize(size)
      f <- outFile
      _ <- removeTargetSize
    } yield f

  private val originProps = Ident.imageProperties.filterNot(id => id == Ident.width || id == Ident.height)

  def originMapping: Ident => Ident = id => id.in("origin")

  private def result(out: File): MapGet[Unit] =
    modify { omap =>
      val map = originProps.foldLeft(LazyMap.fromFile(out)) { (m, id) =>
        m addVirtual(VirtualProperty.defaults.alias(id -> originMapping(id)))
      }
      map ++ omap.mapIdents(originMapping)
    }

  private def processImage(outFile: MapGet[File])(p: Image => Image): MapGet[Boolean] =
    outFile.flatMap { out =>
      val success = result(out).map(_ => true)
      if (out.exists) success
      else image.flatMap {
        case Some(img) =>
          out.parent.createDirectories()
          p(img).output(out.path)(JpegWriter())
          success
        case None =>
          set(LazyMap()).map(_ => false)
      }
    }

  def cover(size: Size, outFile: MapGet[File], method: ScaleMethod = ScaleMethod.FastScale): MapGet[Boolean] =
    processImage(makeOut(size, outFile)) { img =>
      img.cover(size.width, size.height)
    }

  def scaleTo(size: Size, outFile: MapGet[File], method: ScaleMethod = ScaleMethod.Bicubic): MapGet[Boolean] =
    processImage(makeOut(size, outFile)) { img =>
      img.scaleTo(size.width, size.height, method)
    }

  def scaleByFactor(factor: Double, outFile: MapGet[File], method: ScaleMethod = ScaleMethod.Bicubic): MapGet[Boolean] =
    pair(value(Ident.width), value(Ident.height)).flatMap {
      case (Some(w), Some(h)) =>
        scaleTo(Size((w.toInt * factor).toInt, (h.toInt * factor).toInt), outFile, method)
      case _ =>
        unit(false)
    }

  def scaleMaxLen(maxlen: Int, outFile: MapGet[File], method: ScaleMethod = ScaleMethod.Bicubic): MapGet[Boolean] =
    pair(value(Ident.width), value(Ident.height)).flatMap {
      case (Some(w), Some(h)) =>
        val max = math.max(w.toInt, h.toInt)
        if (max <= maxlen) unit(true)
        else {
          val factor = maxlen.toDouble / max
          scaleTo(Size((w.toInt * factor).toInt, (h.toInt * factor).toInt), outFile, method)
        }
      case _ =>
        unit(false)
    }
}

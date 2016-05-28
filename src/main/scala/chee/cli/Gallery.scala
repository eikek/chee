package chee.cli

import chee.properties.FormatPatterns
import com.typesafe.scalalogging.LazyLogging
import java.io.BufferedOutputStream
import java.util.concurrent.atomic.AtomicReference
import better.files.File
import chee.util.mustache.Template
import com.typesafe.config.Config
import Gallery._
import chee.Size
import chee.properties.{Ident, MapGet, Property, VirtualProperty, LazyMap}
import chee.resources.ResourceInfo
import chee.conf._
import chee.CheeDoc
import chee.doc.CheeDocInfo
import chee.util.files._
import chee.util.mustache._
import MapGet._
import LsOptions.{Opts => LsOpts}
import CryptOptions.{Opts => CryptOpts}
import ProcessingOptions.{Opts => ProcOpts}
import java.util.zip.ZipOutputStream

class Gallery extends ScoptCommand with AbstractLs with TransparentDecrypt {

  type T = Opts
  val name = "gallery"
  val defaults = Opts()

  val parser = new Parser with LsOptions[Opts] with CryptOptions[Opts] with ProcessingOptions[Opts] {
    addLsOptions(_ updateLs _)
    addDecryptOptions(_ updateCrypt _, enableOpt = true)

    noteW("\nGallery options")
    concurrent((c, f) => c.copy(concurrent = true))

    opt[Size]("thumb-size") valueName("<width>x<height>") action { (s, c) =>
      c.copy(size = s)
    } text ("The size for thumbnails. Default is 100x100.")

    opt[Double]("scale-factor") action { (m, c) =>
      c.copy(factor = Some(m))
    } text ("Scale images by this factor.")

    opt[Int]("scale-maxlen") action { (n, c) =>
      c.copy(maxLen = Some(n))
    } textW ("Scale image such that the longest side of the image is not more than `maxlen'. Default is set to 1400.")

    opt[Unit]("no-scale") action { (_, c) =>
      c.copy(maxLen = None, factor = None)
    } textW ("Don't scale images and use original files.")

    opt[Unit]("link-original") action { (_, c) =>
      c.copy(linkOriginal = true)
    } textW("Provide links to the original picture. This requires to include all original files into the gallery folder.")

    opt[File]("template") valueName("<file>") action { (f, c) =>
      Template.parse(f.contentAsString) match {
        case Right(t) => c.copy(template = Some(t))
        case Left(msg) => userError(msg)
      }
    } textW("A custom mustache template for creating the gallery page.")

    opt[Unit]("write-template") action { (_, c) =>
      c.copy(writeTemplate = true)
    } textW ("Write the default template to current directory or to the file specified by `--out'. "+
      "If this option is set, only the template is written and nothing else is done.")

    opt[String]("theme") action { (s, c) =>
      c.copy(theme = s)
    } textW ("Choose a css theme. Based on bootstrap this selects a css available from bootswatch. "+
      "Possbible themes are: " + (ResourceInfo.categories - "all").mkString(", ") + ". Default is `bootstrap'.")

    opt[String]("title") action { (t, c) =>
      c.copy(title = Some(t))
    } textW("A custom title for the page.")

    opt[File]("out") valueName("<dir or zip>") action { (f, c) =>
      c.copy(out = Some(f))
    } textW("Write the gallery into the given directory. It is created if non existing. "+
      "If the name ends with `.zip' a temporary folder is used and then zipped into a file "+
      "with this name (it must not exist).")

    addQuery(_ updateLs _)

    checkConfig { cfg =>
      val themes = ResourceInfo.categories - "all"
      if (themes(cfg.theme)) success
      else failure(s"${cfg.theme} is not a known theme")
    }
  }

  def processingAction(cfg: Config, opts: Opts, zip: Option[ZipArchive]): MapGet[Boolean] =
    opts.out match {
      case None => scaleAction(cfg, opts)
      case Some(dir) =>
        def moveFiles(id: Ident, subdir: String, unique: Boolean): MapGet[Boolean] =
          zip match {
            case None => copyAction(id, subdir, dir, unique = false)
            case Some(z) => zipAction(id, subdir, z, unique = false)
          }
        scaleAction(cfg, opts).flatMap {
          case true =>
            moveFiles(thumbnail, "thumbnails", unique = false)
              .flatMap({ _ =>
                if (opts.noScale) unit(true)
                else moveFiles(scaledImage, "images", unique = false)
              })
              .flatMap({ _ =>
                if (!opts.linkOriginal && !opts.noScale) unit(true)
                else moveFiles(Ident.path, "originals", unique = false)
              })
          case false =>
            unit(false)
        }
    }

  def makeContext: MapGet[Value] =
    get.map { m => MapValue(new LazyMapContext(m), false) }

  def writeTemplate(opts: Opts): Unit = {
    val file = opts.out.getOrElse(File("gallery-template.mustache")) match {
      case Directory(f) => f / "gallery-template.mustache"
      case f => f
    }
    file < ResourceInfo.galleryTemplate.contentAsString
    outln(s"Template written: ${file.path}.")
  }

  def findFiles(cfg: Config, opts: Opts, zip: Option[ZipArchive]): Stream[LazyMap] = {
    val progress: MapGet[Unit] = valueForce(Ident.filename).map { name =>
      if (opts.concurrent) out(".")
      else outln(s"Processing $name …")
    }
    val action = progress.flatMap(_ => processingAction(cfg, opts, zip))

    val lsOpts = opts.lsOpts.appendQuery(cfg.getString("chee.queries.gallery-default"))
    val files = findDecrypt(cfg, lsOpts, opts.cryptOpts)
    if (opts.concurrent) MapGet.parfilter(files, action)
    else MapGet.filter(files, action)
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    if (opts.writeTemplate) writeTemplate(opts)
    else {
      val out: Either[ZipArchive, Option[File]] = opts.out match {
        case Some(dir) if dir.getExtension == Some("zip") =>
          outln(s"Creating zip file ${dir.name} …")
          Left(new ZipArchive(new ZipOutputStream(new BufferedOutputStream(dir.newOutputStream))))
        case _ => Right(opts.out)
      }
      outln("Copy resources…")
      val assets = installAssets(cfg, opts.theme, out)

      outln("Create thumbnails and scale images…")
      val files = findFiles(cfg, opts, out.left.toOption)
      val context = assets :: Context(
        "files" -> ListValue(files.map(makeContext.result)),
        "title" -> Value.of(opts.title),
        "link-original" -> Value.of(opts.linkOriginal),
        "projectInfo" -> Value.map(
          "version" -> CheeDocInfo.version,
          "name" -> CheeDocInfo.projectName,
          "homepage" -> CheeDocInfo.homepage
        )
      )

      val html = generateHtml(cfg, opts, context)
      out.left.foreach { z =>
        z.add(html, "index.html")
        z.close()
        html.delete()
      }
      opts.out match {
        case None =>
          CheeDoc.browse(cfg, html)
        case Some(f) =>
          if (opts.concurrent) outln("")
          outln(s"Gallery is at ${f.path}")
      }
    }
  }
}

object Gallery {

  val thumbnail: Ident = 'thumbnail
  val scaledImage: Ident = Ident("scaled-image")

  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    cryptOpts: CryptOpts = CryptOpts(),
    concurrent: Boolean = false,
    size: Size = Size(100),
    maxLen: Option[Int] = Some(1400),
    factor: Option[Double] = None,
    linkOriginal: Boolean = false,
    template: Option[Template] = None,
    theme: String = "bootstrap",
    title: Option[String] = None,
    writeTemplate: Boolean = false,
    out: Option[File] = None
  ) {
    def updateLs(f: LsOpts => LsOpts) = copy(lsOpts = f(lsOpts))
    def updateCrypt(f: CryptOpts => CryptOpts) = copy(cryptOpts = f(cryptOpts))

    lazy val noScale = maxLen == None && factor == None
  }

  def installAssets(cfg: Config, theme: String, out: Either[ZipArchive, Option[File]]): Context = {
    val root: Either[ZipArchive, File] = out.right.map(_ getOrElse (cfg.getFile("chee.tmpdir") / "gallery"))
    val files = for ((kind, urls) <- ResourceInfo.allOf(theme)) yield {
      root.right.foreach(r => (r / "resources" / kind.name).createDirectories())
      val files = urls.map { u =>
        root.fold(
          zip => {
            val name = s"resources/${kind.name}/${u.fileName}"
            zip.add(u, name)
            name
          },
          root => {
            val target = u downloadIn (root / "resources" / kind.name, overwrite = true)
            (root relativize target).toString
          })
      }
      kind.name -> ListValue(files.map(SimpleValue.apply))
    }
    Context(files.get _)
  }

  def generateHtml(cfg: Config, opts: Opts, context: Context): File = {
    val dir = opts.out.filter(_.getExtension != Some("zip")) getOrElse (cfg.getFile("chee.tmpdir") / "gallery")
    val template = opts.template.getOrElse {
      Template.parse(ResourceInfo.galleryTemplate.contentAsString) match {
        case Right(t) => t
        case Left(m) => sys.error(m)
      }
    }
    val target = dir / "index.html"
    if (target.exists) {
      target.delete()
    }
    template.renderTo(context, s => target append s)
    target
  }

  def scaleAction(cfg: Config, opts: Opts): MapGet[Boolean] = {
    val thumb = Thumb.thumbAction(cfg, ProcOpts(), opts.size)
    val scale =
      if (opts.noScale) modify(_.addVirtual(VirtualProperty.defaults.alias(scaledImage -> Ident.path))).flatMap(_ => path.map(Some(_)))
      else Scale.scaleAction(cfg, ProcOpts(), opts.factor, opts.maxLen)
    pair(thumb, scale).flatMap {
      case (ot, os) =>
        val tp: Option[Property] = ot.map(f => Property(thumbnail, f.pathAsString))
        val sp: Option[Property] = os.map(f => Property(scaledImage, f.pathAsString))
        modify { m => m.add(tp).add(sp) } map (_ => os.isDefined)
    }
  }

  def copyAction(id: Ident, subdir: String, out: File, unique: Boolean): MapGet[Boolean] = {
    val dir = out / subdir
    dir.createDirectories()
    valueForce(id).map(File(_)).flatMap { file =>
      val target =
        if (unique) (dir / file.name).makeNonExisting().getOrElse {
          userError(s"Too many equally named files ${(dir / file.name).path}. Rename limit exceeded")
        } else (dir / file.name)
      if (!target.exists) {
        file.copyTo(target)
      }
      modify {
        _.add(id -> (out relativize target).toString)
      } map (_ => true)
    }
  }

  def zipAction(id: Ident, subdir: String, out: ZipArchive, unique: Boolean): MapGet[Boolean] = {
    val entries = new AtomicReference[Set[String]](Set.empty)

    @annotation.tailrec
    def addToZip(f: File, n: Int = 0, max: Int = 9000): String = {
      if (n >= max) sys.error(s"Unable to add ${f.path} to zip file")
      val current = entries.get
      val entryName =
        if (unique) ZipArchive.uniqueEntry(subdir +"/"+ f.name, current)
        else subdir +"/"+ f.name
      val next = current + entryName
      if (entries.compareAndSet(current, next)) {
        out + (f -> entryName)
        entryName
      } else {
        println("try again…")
        addToZip(f, n + 1, max)
      }
    }
    valueForce(id).map(File(_)).flatMap { file =>
      val target = addToZip(file)
      modify {
        _.add(id -> target)
      } map (_ => true)
    }
  }

  case class LazyMapContext(lm: LazyMap) extends Context with LazyLogging {
    def find(key: String): (Context, Option[Value]) = {
      key match {
        case "_format" =>
          def format(pattern: String): ContextGet[String] =
            FormatPatterns.parse(pattern) match {
              case Right(p) =>
                val (next, v) = p.run(lm)
                ContextGet.setHead(LazyMapContext(next)).map(_ =>
                  v.fold(identity, { msg =>
                    logger.warn(s"Error evaluating pattern '$pattern': $msg")
                    ""
                  }))
              case Left(msg) =>
                logger.warn(s"Invalid format pattern '$pattern': $msg")
                ContextGet.unit("")
            }

          (this, Some(Value.lambda(s => format(s.inner.foldLeft("")(_ + _.asString)))))
        case _ =>
          if (!Ident.validate(key)) (this, None)
          else {
            val (next, v) = lm(key)
            (new LazyMapContext(next), v.map(_.value))
          }
      }
    }
  }
}

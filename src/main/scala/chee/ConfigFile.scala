package chee

import better.files._
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions, ConfigValue, ConfigList, ConfigValueFactory}
import com.typesafe.scalalogging.LazyLogging

final class ConfigFile private (val file: File, val config: Config) extends LazyLogging {
  private val renderOptions =
    ConfigRenderOptions.defaults()
      .setJson(false)
      .setOriginComments(false)

  def save(): Unit = {
    logger.debug(s"Save system config file: ${file.path}")
    val content = config.root().render(renderOptions)
    if (!file.exists) file.parent.createDirectories()
    file.overwrite(content)
  }

  def delete(): Unit =
    if (file.exists) {
      logger.trace(s"Delete system config file: ${file.path}")
      file.delete()
    }

  def setValue(key: String, value: ConfigValue): ConfigFile =
    new ConfigFile(file, config.withValue(key, value))

  def set[V](key: String, value: V)(implicit conv: ConfigFile.Conversion[V]) =
    setValue(key, conv.make(value))

  def getInt(key: String): Int = config.getInt(key)

  def getString(key: String): String = config.getString(key)

  def get[V](key: String)(implicit conv: ConfigFile.Conversion[V]): V =
    conv.read(config.getValue(key))

  def exists(key: String): Boolean =
    config.hasPath(key)

  def getOpt[V](key: String)(implicit conv: ConfigFile.Conversion[V]): Option[V] =
    if (exists(key)) Some(get[V](key))
    else None

  override def toString(): String =
    s"ConfigFile(${file.path})"
}

object ConfigFile {

  trait Conversion[V] {
    def make(v: V): ConfigValue
    def read(v: ConfigValue): V
  }

  def apply(file: File): ConfigFile = {
    val config =
      if (file.exists) ConfigFactory.parseFile(file.path.toFile)
      else ConfigFactory.empty()
    new ConfigFile(file, config)
  }

  object Conversion {
    import scala.collection.JavaConverters._

    def apply[V](m: V => ConfigValue, r: ConfigValue => V): Conversion[V] =
      new Conversion[V] {
        def make(v: V) = m(v)
        def read(v: ConfigValue) = r(v)
      }

    implicit val fromString: Conversion[String] = Conversion(
      s => ConfigValueFactory.fromAnyRef(s),
      cv => cv.unwrapped().toString)

    implicit val fromInt: Conversion[Int] = Conversion(
      i => ConfigValueFactory.fromAnyRef(i),
      cv => cv.unwrapped().asInstanceOf[Int])

    implicit val fromBool: Conversion[Boolean] = Conversion(
      b => ConfigValueFactory.fromAnyRef(b),
      cv => cv.unwrapped().asInstanceOf[Boolean])

    implicit def fromSeq[A](implicit mv: Conversion[A]): Conversion[Seq[A]] =
      Conversion(
        seq => ConfigValueFactory.fromIterable(seq.map(mv.make).asJava),
        cv => cv.asInstanceOf[ConfigList].asScala.map(mv.read).toList)

    implicit val fromMap: Conversion[Map[String, Any]] = Conversion(
      m => ConfigValueFactory.fromMap(m.asJava),
      cv => cv.unwrapped().asInstanceOf[java.util.Map[String, Any]].asScala.toMap)
  }
}

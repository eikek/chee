package chee.conf

import better.files._
import com.typesafe.config.{ Config, ConfigFactory, ConfigValue, ConfigValueFactory }

object CheeConf {
  // for scala.concurrent.ExecutionContext.Implicits.global
  System.setProperty("scala.concurrent.context.numThreads", "x1.5")

  private val debug = {
    System.getProperty("chee.debugConfig", "false") equalsIgnoreCase "true"
  }

  lazy val defaultConfig = loadDefault()

  def loadDefault(): Config = load(Directories.makeDefault)

  def load(dirs: Directories): Config = {
    val userCfg = userConfigOverride orElse dirs.userConfig.map(parse)
    val config =
      dirs.initial withFallback
      userCfg.getOrElse(ConfigFactory.empty) withFallback
      ConfigFactory.defaultApplication

    val overrides = overrideConfig(dirs).withFallback(ConfigFactory.defaultOverrides())
    val ref = ConfigFactory.defaultReference()

    ((overrides withFallback config) withFallback ref).resolve
  }

  private def overrideConfig(dirs: Directories) = {
    val overrides = List(
      "user.dir" -> dirs.userDir,
      "chee.configdir" -> dirs.configDir,
      "chee.workingdir" -> dirs.workingDir,
      "chee.repo.root" -> dirs.repoRoot)
    overrides.foldLeft(ConfigFactory.empty) {
      case (cfg, (path, Some(dir))) =>
        debug(s"Overriding config value $path -> ${dir.path}")
        cfg.withValue(path, cfgValue(dir.pathAsString))
      case (cfg, _) => cfg
    }
  }

  private def debug(msg: String): Unit = {
    if (debug) println(msg)
  }

  private def parse(file: File): Config = {
    debug(s"Parsing config file: ${file.path}")
    ConfigFactory.parseFile(file.toJava)
  }

  private def cfgValue(a: AnyRef): ConfigValue =
    ConfigValueFactory.fromAnyRef(a)

  private def userConfigOverride: Option[Config] =
    Option(System.getProperty("chee.config")).map(File(_)) match {
      case Some(f) if f.exists =>
        debug("Use config file from system property")
        Some(parse(f))
      case Some(f) =>
        debug(s"Use config file from system property ${f.path}, but it does not exist. Creating empty config.")
        Some(ConfigFactory.empty())
      case _ => None
    }
}

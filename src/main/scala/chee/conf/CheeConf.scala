package chee.conf

import better.files._
import chee.ConfigFile
import com.typesafe.config.{ Config, ConfigFactory, ConfigValue, ConfigValueFactory }


object CheeConf {
  // for scala.concurrent.ExecutionContext.Implicits.global
  System.setProperty("scala.concurrent.context.numThreads", "x1.5")

  private val debug = {
    System.getProperty("chee.debugConfig", "false") equalsIgnoreCase "true"
  }

  val defaultConfig = loadDefault()

  def loadDefault(): Config = load(Directories.makeDefault)

  def load(dirs: Directories): Config = load(dirs.userConfig, initialConfig(dirs))

  def load(userConfigFile: Option[File], overrides: Config): Config = {
    val userConfig = userConfigOverride orElse userConfigFile.map(parse) match {
      case Some(cfg) => overrides withFallback cfg
      case _ => overrides
    }
    ConfigFactory.load(userConfig withFallback ConfigFactory.defaultApplication)
  }

  def initialConfig(dirs: Directories) = {
    val overrides = List(
      "user.dir" -> dirs.userDir,
      "chee.configdir" -> dirs.configDir,
      "chee.workingdir" -> dirs.workingDir,
      "chee.repo.root" -> dirs.repoRoot)
    overrides.foldLeft(dirs.initial) {
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

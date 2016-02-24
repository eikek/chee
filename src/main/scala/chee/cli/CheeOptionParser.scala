package chee.cli

abstract class CheeOptionParser[T](name: String) extends scopt.OptionParser[T](name) {
  note("Note: Use `chee help' for detailed help topics. This is a quick option\noverview.\n")

  help("usage") text("Show usage")

  override def showTryHelp(): Unit = {
    Console.err.println("Try `chee help' for detailed help topics or `--usage' for usage information.")
  }
}

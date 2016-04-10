package chee.cli

abstract class CheeOptionParser[T](name: String) extends scopt.OptionParser[T](name) {
  noteW("Note: Use `chee help' for detailed help topics. This is a quick option overview.\n")

  help("usage") text("Show usage")

  def noteW(text: String) = note(wrapLines(70)(text))


  private def prefixLines(text: String, prefix: String): String =
    text.replace("\n", s"\n$prefix")

  override def showTryHelp(): Unit = {
    Console.err.println("Try `chee help' for detailed help topics or `--usage' for usage information.")
  }

  implicit class OptionDefOps[A: scopt.Read, C](odef: scopt.OptionDef[A, C]) {
    def textW(text: String) = odef.text(prefixLines(wrapLines(62)(text), "        "))
  }
}

package chee.it

import chee.cli._
import chee.it.CommandSetup.Setup

trait FindHelper {
  self: CommandSetup =>

  def find = new Find with BufferOut

  def findLisp(setup: Setup, q: String = "") =
    if (q.isEmpty) find.run(setup, "-a", "-p", "lisp")
    else find.run(setup, "-a", "-p", "lisp", q)
}

package ateams.frontend

import caos.frontend.Site.initSite
import ateams.syntax.Program
import ateams.syntax.Program.ASystem

/** Main function called by ScalaJS' compiled javascript when loading. */
object Main {
  def main(args: Array[String]):Unit =
    initSite[ASystem](CaosConfig)
}
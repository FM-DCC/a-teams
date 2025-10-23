package ateams.frontend

import caos.frontend.Configurator.*
import caos.frontend.{Configurator, Documentation}
import caos.view.*
import ateams.backend.*
import ateams.syntax.Program.{ASystem, preProcess}
import ateams.syntax.{Program, Show}
import ateams.backend.Semantics.St

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[ASystem]:
  val name = "Animator of A-Teams"
  override val languageName: String = "Input program"

  /** Parser, converting a string into a System in A-Teams */
  val parser =
    ateams.syntax.Parser.parseProgram
      .andThen(preProcess)

  /** Examples of programs that the user can choose from. The first is the default one. */
  val examples = List(
    "ex0"
      -> "act\n  default: sync, 1->1;\n  coin; coffee;\n  pub: fifo @ snd, 1->0;\n  // other supported examples:\n  // fifo\n  // unsorted\n  // fifo @ rcv,\n  // fifo @ snd\n  // fifo @ snd-rcv\n  // fifo @ global\n  // 1..3 -> 4...5\n  // 1 -> 0..*\n\nlet\n CM = coin!.tau.coffee?.CM\n CS = pub!.pub!.coin?.coffee!.CS\nin\n CM||CS"
      -> "Experiment",
    "ex1"
      -> "act\n  default: sync, 1->1;\n  coin; coffee;\n  pub: 1->0;\n  // other supported examples:\n  // fifo\n  // unsorted\n  // fifo @ rcv,\n  // fifo @ snd\n  // fifo @ snd-rcv\n  // fifo @ global\n  // 1..3 -> 4...5\n  // 1 -> 0..*\n\nlet\n CM = coin!.tau.coffee?.CM\n CS = pub!.coin?.coffee!.CS\nin\n CM||CS"
      -> "Example of a program in A-Teams",
    "race"
      -> "act\n\tdefault: sync;\n  start: 1->2;\n  finish: 1->1;\n  run: 1->0;\n\nlet\n Ctr = start!.finish?.finish?.Ctr\n R = start?.run!.finish!.R\nin\n Ctr || R || R"
      -> "Ususal runner example",
  )

  /** Description of the widgets that appear in the dashboard. */
  val widgets = List(
     "View pretty data" -> view[ASystem](Show.apply, Code("haskell")), //.moveTo(1),
//    "View structure" -> view(Show.mermaid, Mermaid),
     "Run semantics" -> steps(e=>St(e,Map(),Map()), Semantics, x=>Show/*.short*/(x), Show(_), Text).expand,
     "Build LTS" -> lts((e:ASystem)=>St(e,Map(),Map ()), Semantics, x=>"", Show(_)),
//     "Build LTS (explore)" -> ltsExplore(e=>e, Semantics, x=>Show(x.main), _.toString),
//    "Find strong bisimulation (given a program \"A ~ B\")" ->
//      compareStrongBisim(Semantics, Semantics,
//        (e: CCSSystem) => CCSSystem(e.defs, e.main, None),
//        (e: CCSSystem) => CCSSystem(e.defs, e.toCompare.getOrElse(Program.Term.End), None),
//        Show.justTerm, Show.justTerm, _.toString),
  )

  //// Documentation below

  override val footer: String =
    """Simple animator of A-Teams, using the
      | CAOS libraries. Source code available online:
      | <a target="_blank" href="https://github.com/FM-DCC/a-teams">
      | https://github.com/FM-DCC/a-teams</a>.""".stripMargin


  override val documentation: Documentation = List(
//    languageName -> "More information on the syntax of A-Teams" ->
//      "...",
  )

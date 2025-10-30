package ateams.frontend

import caos.frontend.Configurator.*
import caos.frontend.{Configurator, Documentation}
import caos.view.*
import ateams.backend.*
import ateams.syntax.Program.{ASystem, preProcess}
import ateams.syntax.{Program, Show}
import ateams.backend.Semantics.St
import caos.sos.SOS

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
//    "ex0"
//      -> "acts\n  default: fifo, 1->1;\n  coin; coffee;\n\nproc\n CM = coin!cs.coffee?.CM\n CS = coin?.coffee!.CS\nin\n CM||cs:CS"
//      -> "Experiment",
    "coffee-sync"
      -> "acts\n  default: sync, 1->1;\n  coin; coffee;\n  pub: 1->0;\n  // other supported examples:\n  // fifo\n  // unsorted\n  // fifo @ rcv,\n  // fifo @ snd\n  // fifo @ snd-rcv\n  // fifo @ global\n  // 1..3 -> 4...5\n  // 1 -> 0..*\n\nproc\n CM = coin!.tau.coffee?.CM\n CS = pub!.coin?.coffee!.CS\ninit\n CM||CS"
      -> "Simple coffee with synchronous channels",
    "coffee-async"
      -> "acts\n  default: fifo, 1->1;\n  coin; coffee;\n  pub: 1->0;\n\nproc\n CM = coin!cs.coffee?.CM\n CS = pub!.coin?.coffee!cm.CS\ninit\n cm:CM||cs:CS"
      -> "Asynchronous version of the coffee machine with FIFO channels",
    "race-sync"
      -> "acts\n\tdefault: sync;\n  start: 1->2;\n  finish: 1->1;\n  run: 1->0;\n\nproc\n Ctr = start!.finish?.finish?.Ctr\n R = start?.run!.finish!.R\ninit\n Ctr || R || R"
      -> "Ususal runner example",
    "race-finish@snd"
      -> "acts\n\tdefault: sync;\n  start: 1->2;\n  finish: 2->1, fifo @ snd;\n  run: 1->0;\n\nproc\n Ctr = start!.finish?r1,r2.Ctr\n R = start?.run!.finish!.R\ninit\n  Ctr || r1:R || r2:R"
      -> "Race async experiment - finish sends asynchronously, with a buffer for each sender (runner)",
    "race-finish@rcv"
      -> "acts\n\tdefault: sync;\n  start: 1->2;\n  finish: 2->1, fifo @ rcv;\n  run: 1->0;\n\nproc\n Ctr = start!.finish?.Ctr\n R = start?.run!.finish!c.R\ninit\n  c:Ctr || R || R"
      -> "Race async experiment - finish sends asynchronously, with a single buffer for the receiver (controller)",
    "race-async"
      -> "acts\n\tdefault: fifo;\n  start: 1->2;\n  finish: 2->1;\n  run: 1->0;\n\nproc\n Ctr = start!r1,r2.finish?.Ctr\n R = start?.run!.finish!c.R\ninit\n  c:Ctr || r1:R || r2:R"
      -> "Race async - all channels are asynchronous",
  )

  /** Description of the widgets that appear in the dashboard. */
  val widgets = List(
     "View pretty data" -> view[ASystem](Show.apply, Code("haskell")), //.moveTo(1),
//    "View structure" -> view(Show.mermaid, Mermaid),
     "Well-formed?" -> view[ASystem](x => ateams.backend.TypeCheck.pp(x), Text).expand,
     "Run semantics" -> steps(e=>St(e,Map()), Semantics, x=>Show/*.short*/(x), Show(_), Text).expand,
     "Build LTS" -> lts((e:ASystem)=>St(e,Map ()), Semantics, Show.showBuffers, Show(_)),
     "Local component" ->
       viewMerms((sy: ASystem) =>
          for (nm,proc) <- sy.main.toList yield
              s"$nm:${Show(proc)}" -> SOS.toMermaid[Program.Act,Program.Proc](
                // SOS semantics
                new SOS[Program.Act,Program.Proc] {
                  override def next[A >: Program.Act](p: Program.Proc): Set[(A, Program.Proc)] =
                    Semantics.nextProc(p)(using St(sy,Map())).asInstanceOf[Set[(A,Program.Proc)]]
                }  ,
                proc, // initial state
                x=>"", // displaying states
                Show(_), // displaying labels
                80 // max size
              )).expand,
     "Number of states and edges"
      -> view((sy:ASystem) => {
          val (st,eds,done) = SOS.traverse(Semantics,St(sy,Map()),2000)
          //s"== Full state space ==" +
          (if !done then s"(Stopped after traversing 2000 states)"
           else s"States: ${st.size}\nEdges: $eds")
        },
        Text),

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

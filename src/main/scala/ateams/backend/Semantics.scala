package ateams.backend

import caos.sos.SOS
import caos.common.Multiset as MSet
import ateams.backend.Semantics.St
import ateams.syntax.Program
import Program.*
import Proc.*
import ateams.syntax.Program.Act.Out

import scala.collection.immutable
import scala.collection.mutable.Queue

/** Small-step semantics for both commands and boolean+integer expressions.  */
object Semantics extends SOS[Act,St]:

  case class St(sys: ASystem, fifos:Map[Loc,Queue[String]], msets:Map[Loc,MSet[String]])

  case class Loc(snd: Option[String], rcv: Option[String])
  type Defs = Map[String,Proc]
  type Procs = Map[String,Proc]

  /** What are the set of possible evolutions (label and new state) */
  def next[A>:Act](st: St): Set[(A, St)] =
    for (a,ps) <- next(st.sys.main)(using st)
    yield a -> st.copy(sys = st.sys.copy(main = ps))

  def nextOld(procs:Procs)(using st:St): Set[(Act,Procs)] =
    for (n,p) <- procs.toSet; (a,p2) <- nextProc(p)
    yield a -> (procs+(n->p2))

  def next(procs:Procs)(using st:St): Set[(Act,Procs)] =
    val canGo =
      for (n,p) <- procs.toSet; (a,p2) <- nextProc(p)
      yield a -> (n->p2)
    nextSync(canGo) ++
    nextTau(canGo)

  def nextTau(canGo: Set[(Act,(String,Proc))])(using st:St): Set[(Act,Procs)] =
    for (a,(n,p)) <- canGo if a == Act.IO("tau")
      yield  a -> (st.sys.main + (n->p))


  private def getActName(a:Act): String = a match
    case Act.In(s) => s
    case Act.Out(s) => s
    case Act.IO(s) => s

  /////////////////////
  def nextSync(canGo: Set[(Act,(String,Proc))])(using st:St): Set[(Act,Procs)] = {
    var syncsMap = Map[String,(List[(String,Proc)],List[(String,Proc)])]()
    for (a,(n,p)) <- canGo
        if st.sys.msgs.get(getActName(a)).flatMap(_.st).getOrElse(MsgInfo.defaultST) == SyncType.Sync
        do {
      a match {
        case Act.In(s) if syncsMap.contains(s) =>
          syncsMap = syncsMap + (s -> (syncsMap(s)._1,(n,p)::syncsMap(s)._2))
        case Act.In(s) =>
          syncsMap = syncsMap + (s -> (Nil,List(n->p)))
        case Act.Out(s) if syncsMap.contains(s) =>
          syncsMap = syncsMap + (s -> ((n,p)::syncsMap(s)._1,syncsMap(s)._2 ))
        case Act.Out(s) =>
          syncsMap = syncsMap + (s -> (List(n->p),Nil))
        case _ => {}
      }
    }

    def arit(act: String): (Intrv,Intrv) =
      st.sys.msgs.get(act).flatMap(_.arity).getOrElse(MsgInfo.defaultArity)

    val combs: Iterable[(String, List[(String,Proc)], List[(String,Proc)])] =
      (for
        (a,(snds,rcvs)) <- syncsMap
      yield
        val smin = arit(a)._1._1
        val smax = arit(a)._1._2.getOrElse(snds.size) min snds.size
        val rmin = arit(a)._2._1
        val rmax = arit(a)._2._2.getOrElse(rcvs.size) min rcvs.size
        for ssize <- smin to smax
            rsize <- rmin to rmax
            sset <- snds combinations ssize
            rset <- rcvs combinations rsize
        yield
          (a,sset,rset)
        ).flatten

    val combss = combs.toSet
    for (a,snds,rcvs) <- combs.toSet yield
      Act.IO(a) -> (st.sys.main ++ snds ++ rcvs)

  }

  def nextProc(p:Proc)(using st:St): Set[(Act,Proc)] = p match
    case End => Set()
    case ProcCall(p) => nextProc(st.sys.defs.getOrElse(p,End))
    case Prefix(act,p) => Set(act -> p)
    case Choice(p1,p2) =>
      nextProc(p1) ++ nextProc(p2)
    case Par(p1, p2) =>
      val nx1 = nextProc(p1)
      val nx2 = nextProc(p2)
      (for (n,s)<-nx1 yield n->Par(s,p2)) ++
      (for (n,s)<-nx2 yield n->Par(p1,s))

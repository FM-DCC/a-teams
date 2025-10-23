package ateams.backend

import caos.sos.SOS
import caos.common.Multiset as MSet
import ateams.backend.Semantics.St
import ateams.syntax.{Program, Show}
import Program.*
import Proc.*
import ateams.syntax.Program.Act.Out

//import scala.collection.{immutable, mutable}
import scala.collection.immutable.Queue
import scala.language.implicitConversions

/** Small-step semantics for both commands and boolean+integer expressions.  */
object Semantics extends SOS[Act,St]:

  case class St(sys: ASystem, fifos:Map[Loc,Queue[String]], msets:Map[Loc,MSet[String]])

  case class Loc(snd: Option[String], rcv: Option[String])
  type Defs = Map[String,Proc]
  type Procs = Map[String,Proc]

  /** What are the set of possible evolutions (label and new state) */
  def next[A>:Act](st: St): Set[(A, St)] =
    nextPr(st.sys.main)(using st)
//    for (a,ps) <- next(st.sys.main)(using st)
//    yield a -> st.copy(sys = st.sys.copy(main = ps))

  def nextOld(procs:Procs)(using st:St): Set[(Act,Procs)] =
    for (n,p) <- procs.toSet; (a,p2) <- nextProc(p)
    yield a -> (procs+(n->p2))

  def nextPr[A>:Act](procs:Procs)(using st:St): Set[(A,St)] =
    val canGo =
      for (n,p) <- procs.toSet; (a,p2) <- nextProc(p)
      yield a -> (n->p2)
    updateSt(nextTau(canGo)) ++
    updateSt(nextSync(canGo)) ++
    nextSend(canGo)

  def updateSt(aps: Set[(Act,Procs)])(using st:St): Set[(Act,St)] =
    for (a,ps) <- aps yield
      a -> st.copy( sys = st.sys.copy( main = st.sys.main ++ ps))


  ///////////////////////////////////
  // Semantics of the "tau" action //
  ///////////////////////////////////

  def nextTau(canGo: Set[(Act,(String,Proc))])(using st:St): Set[(Act,Procs)] =
    for (a,(n,p)) <- canGo if a == Act.IO("tau")
      yield  a -> Map(n->p) // just the updates

  ///////////////////////////////////////
  // Semantics of synchronous messages //
  ///////////////////////////////////////

  def nextSync(canGo: Set[(Act,(String,Proc))])(using st:St): Set[(Act,Procs)] = {
    var syncsMap = Map[String,(List[(String,Proc)],List[(String,Proc)])]()
    for (a,(n,p)) <- canGo if stype(a) == SyncType.Sync
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
      Act.IO(a) -> (snds ++ rcvs).toMap // just the updates
  }

  //////////////////////////////////////////
  // Semantics of the FIFO/unsorted sends //
  //////////////////////////////////////////

  def nextSend(canGo: Set[(Act,(String,Proc))])(using st:St): Set[(Act,St)] =
    println(s"can send? ${canGo.mkString("\n")}\nstype(${canGo.tail.head._1}) = ${stype(canGo.tail.head._1)}")
    (for case (a@Act.Out(s), (n, p)) <- canGo
        if isFifo(stype(s)) //  List(SyncType.Fifo,SyncType.Unsorted) contains stype(a)
     yield {
       println(s"SENDING $a...")
       val queue = st.fifos.getOrElse(getLoc(a,Some(n),None),Queue[String]())
         .enqueue(getActName(a))
       a -> st.copy( sys = st.sys.copy( main = st.sys.main + (n->p)),
                     fifos = st.fifos + (getLoc(a,Some(n),None) -> queue))
     }
      )++
    (for case (a@Act.Out(s), (n, p)) <- canGo
             if isUnsorted(stype(s)) //  List(SyncType.Fifo,SyncType.Unsorted) contains stype(a)
     yield
      a -> st.copy( sys = st.sys.copy( main = st.sys.main + (n->p)),
        msets = st.msets + (getLoc(a,Some(n),None) ->
          (st.msets.getOrElse(getLoc(a,Some(n),None),MSet()) + getActName(a))))
    )


  ////////////////////////////
  // Semantics of a process //
  ////////////////////////////

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


  //////////////////////////
  // Auxiliarly functions //
  //////////////////////////

  private implicit def getActName(a:Act): String = a match
    case Act.In(s) => s
    case Act.Out(s) => s
    case Act.IO(s) => s

  private def getLoc(act: String, snd:Option[String], rcv: Option[String])(using st:St): Loc =
    stype(act) match
      case SyncType.Sync => sys.error(s"Cannot get buffer of sync message \"$act\"")
      case SyncType.Fifo(locInfo)     => getLocInfo(locInfo,act,snd,rcv)
      case SyncType.Unsorted(locInfo) => getLocInfo(locInfo,act,snd,rcv)

  private def getLocInfo(linfo: LocInfo, act:String,
                         snd: Option[String],
                         rcv: Option[String]): Loc = (linfo,snd,rcv) match
    case (LocInfo(true,true),Some(s),Some(r)) => Loc(snd,rcv)
    case (LocInfo(true,false),Some(s),_) => Loc(snd,None)
    case (LocInfo(false,true),_,Some(r)) => Loc(None,rcv)
    case (LocInfo(false,false),_,_) => Loc(None,None)
    case _ => sys.error(s"Missing information for channel \"$act\": type ${Show(linfo)}, got $snd->$rcv.")


  private def arit(act: String)(using st:St): (Intrv, Intrv) =
    st.sys.msgs.get(act).flatMap(_.arity).getOrElse(MsgInfo.defaultArity)

  private def stype(act: String)(using st:St) =
    st.sys.msgs.get(act).flatMap(_.st).getOrElse(MsgInfo.defaultST)

  private def isFifo(syncType: Program.SyncType): Boolean =
    syncType match
      case SyncType.Fifo(_) => true
      case _ => false

  private def isUnsorted(syncType: Program.SyncType): Boolean =
    syncType match
      case SyncType.Unsorted(_) => true
      case _ => false
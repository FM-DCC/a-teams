package ateams.backend

import Semantics.{St, aritSys}
import ateams.syntax.Program
import ateams.syntax.Program.{ASystem, Act, MsgInfo, Proc, ProcName, SyncType, LocInfo}
import ateams.syntax.Show

object TypeCheck:
  type Errors = Set[String]

  def pp(sy:ASystem) =
    val err = check(sy)
    if err.isEmpty then "Well-formed"
    else "Not well-formed:\n"+err.map(x => s" - $x").mkString("\n")
  def check(st:St): Errors = check(st.sys)
  def check(sy:ASystem): Errors =
    sy.defs.toSet.map(x => check(x._2)(using sy, x._1)).flatten
  def check(p:Proc)(using sy:ASystem, pname:ProcName): Errors =
    p match {
      case Proc.End => Set()
      case Proc.ProcCall(p) => Set()
      case Proc.Choice(p1, p2) => check(p1)++check(p2)
      case Proc.Par(p1, p2) => check(p1)++check(p2)
      case Proc.Prefix(Act.In(act,from), p) => check(p) ++
        (sy.msgs.get(act) match
          case None => Set(s"[$pname] action $act not found.")
          case Some(mi:MsgInfo) => mi match
            case MsgInfo(Some((i1,i2)), Some(SyncType.Sync)) =>
              if from.nonEmpty && !Semantics.inInterval(from.size,i1) then
                Set(s"[$pname] Trying to receive '$act' from {${from.mkString}} but expected # in interval {${Show.showIntrv(i1)}}.")
              else Set()
            case MsgInfo(Some((i1,i2)), Some(SyncType.Async(LocInfo(snd, rcv),_))) =>
              if from.nonEmpty && (!snd) then
                Set(s"[$pname] Receiving $act from ${from.mkString}, but no sources should be specified.")
              else if from.isEmpty && snd && i1!=(0,Some(0)) then
                Set(s"[$pname] Receiving $act with no sources, but expected some targets (in {${Show.showIntrv(i1)}}).")
              else if from.nonEmpty && snd && !Semantics.inInterval(from.size,i1) then
                Set(s"[$pname] Trying to receive '$act' from {${from.mkString}} but expected # in interval {${Show.showIntrv(i1)}}.")
              else Set()
            case _ => Set(s"Unexpected message info: ${Show(mi)}")
          )
      case Proc.Prefix(Act.Out(act,to), p) => check(p) ++
        (sy.msgs.get(act) match
          case None => Set(s"[$pname] action $act not found.")
          case Some(mi:MsgInfo) => mi match
            case MsgInfo(Some((i1,i2)), Some(SyncType.Sync)) =>
              if to.nonEmpty && !Semantics.inInterval(to.size,i2) then
                Set(s"[$pname] Trying to send '$act' to {${to.mkString}} but expected # in interval {${Show.showIntrv(i2)}}.")
              else Set()
            case MsgInfo(Some((i1,i2)), Some(SyncType.Async(LocInfo(snd, rcv),_))) =>
              if to.nonEmpty && (!rcv) then
                Set(s"[$pname] Sending $act to ${to.mkString}, but no destination should be specified.")
              else if to.isEmpty && rcv && i2!=(0,Some(0)) then
                Set(s"[$pname] Sending $act with no destination, but expected some targets (in ${Show.showIntrv(i2)}).")
              else if to.nonEmpty && rcv && !Semantics.inInterval(to.size,i2) then
                Set(s"[$pname] Trying to send '$act' to {${to.mkString}} but expected # in interval {${Show.showIntrv(i2)}}.")
              else Set()
            case _ => Set(s"Unexpected message info: ${Show(mi)}")
          )
      case Proc.Prefix(Act.IO(_,_,_),p) => check(p)
    }

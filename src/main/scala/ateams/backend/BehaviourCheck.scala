package ateams.backend

import Semantics.{St, nextProc}
import ateams.syntax.Program.Act
import ateams.syntax.Show

object BehaviourCheck:

  /** Traverse the state space, while collecting all states and edges, and collect errors/warning regarding:
   *    - deadlock
   *    - sync receptiveness
   *    - sync responsiveness
   *    - async responsiveness
   *    - too large
   *
   * @param rx input initial state
   * @param max maximum edges traversed
   * @return traversed states (processes and buffers), number of edges, list of warnings
   */
  def randomWalk(st:St, max:Int=5000): (Set[St],Int,List[String]) =
    def aux(next:Set[St], done:Set[St],
            nEdges:Int, probs:List[String],
            limit:Int): (Set[St],Int,List[String]) =
      if limit <=0 then
        // error 1: too big
        return (done,nEdges, s"Reached limit - traversed +$max edges."::probs)
      next.headOption match
        case None =>
          (done, nEdges, probs) // success
        case Some(st) if done contains st =>
          aux(next-st,done,nEdges,probs,limit)
        case Some(st) => //visiting new state
          val next2 = Semantics.next(st)
          var newProbs = checkState(st,next2) // check here for problems
          val moreSts = next2.map(_._2)
          val nNewEdges = moreSts.size
          if moreSts.isEmpty then newProbs =  s"Deadlock found: ${Show.oneLine(st)}" :: newProbs

          aux((next-st)++moreSts,
              done+st,
              nEdges+nNewEdges,
              newProbs ::: probs,
              limit-nNewEdges)

    aux(Set(st), Set(), 0, Nil, max)

  def checkState(st:St, next: Set[(Act,St)]): List[String] = {
    val acts = next.map(x => Semantics.getActName(x._1))
    // sync receptiveness
    val canSend =
      for (agName,proc) <- st.sys.main; case (Act.Out(aName,to),_) <- nextProc(proc)(using st)
      yield (agName,aName)
    val badSyncRecep = for (agName,aName) <- canSend if !acts.contains(aName) yield
      s"[strong-receptiveness] $agName can send $aName, but the system cannot send it: ${Show.oneLine(st)}."
    badSyncRecep.toList
  }



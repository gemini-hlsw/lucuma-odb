// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Eq
import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.SequenceType
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepConfig

/**
 * Internal structures required for completion state tracking in an observation.
 * The Completion.State is built up completed step by completed step using a
 * Completion.State.Builder.
 */
object Completion {

  // Tuple of instrument dynamic config, D, and the `StepConfig` (science vs
  // gcal vs dark etc.)
  type StepMatch[D] = (D, StepConfig)

  // List of steps that make up an atom.
  type AtomMatch[D] = List[StepMatch[D]]

  object AtomMatch:
    def fromProtoAtom[D](atom: ProtoAtom[ProtoStep[D]]): AtomMatch[D] =
      atom.steps.map(s => (s.value, s.stepConfig)).toList

  /**
   * AtomMap associates atom configurations with a count of executed instances
   * of the atom.  This is used to filter executed atoms when producing the
   * sequence.
   *
   * @tparam D dynamic instrument configuration type
   */
  opaque type AtomMap[D] = Map[AtomMatch[D], PosInt]

  object AtomMap {

    def Empty[D]: AtomMap[D] = Map.empty

    def from[D](seq: (AtomMatch[D], PosInt)*): AtomMap[D] =
      Map.from(seq)

    extension [D](m: AtomMap[D]) {

      def increment(k: AtomMatch[D]): AtomMap[D] =
        m.updatedWith(k):
          case None    => PosInt.unsafeFrom(1).some
          case Some(p) => PosInt.from(p.value + 1).toOption

      def decrement(k: AtomMatch[D]): AtomMap[D] =
        m.updatedWith(k)(_.flatMap(p => PosInt.from(p.value - 1).toOption))

      def contains(k: AtomMatch[D]): Boolean =
        m.contains(k)

      def isEmpty: Boolean =
        m.isEmpty

      def toMap: Map[AtomMatch[D], PosInt] =
        m

      /**
       * Match the given `ProtoAtom[ProtoStep[D]]` against the completed atoms.
       * If there is at least one matching completed atom, `true` is returned
       * and a copy of the completed atom map with one fewer instance.
       * Otherwise `false` is returned along with this completed atom map.
       */
      def matchAtom(a: ProtoAtom[ProtoStep[D]]): (AtomMap[D], Boolean) =
        val k = AtomMatch.fromProtoAtom(a)
        if (contains(k)) (decrement(k), true) else (m, false)

    }

    // A state-machine used in creating an AtomMap.
    trait Builder[D]:

      // Reset the state, discarding any partially matched atom information.
      def reset: Builder[D]

      // Finish building and produce the AtomMap.
      def build: AtomMap[D]

      // Incorporate the next executed step
      def next(aid: Atom.Id, count: NonNegShort, step: StepMatch[D]): Builder[D]

    object Builder {

      def init[D]: Builder[D] = Reset(AtomMap.Empty)

      // A builder state that remembers the matched completed atoms, but is not
      // currently in progress matching a new aotm.
      private case class Reset[D](completed: AtomMap[D]) extends Builder[D]:
        override def reset: Builder[D] = this

        override def next(aid: Atom.Id, count: NonNegShort, step: StepMatch[D]): Builder[D] =
          InProgress(aid, count, List(step), completed)

        override def build: AtomMap[D] = completed

      // A builder state for in progress matching of an atom.  It remembers the
      // completed atoms we've previously matched along with in progress steps
      // toward completing the next atom.
      private case class InProgress[D](
        inProgressAtomId: Atom.Id,
        inProgressCount:  NonNegShort,
        inProgressSteps:  AtomMatch[D],
        completed:        AtomMap[D]
      ) extends Builder[D]:

        override def reset: Builder[D] = Reset(build)

        private def addStep(step: StepMatch[D]): InProgress[D] =
          copy(inProgressSteps = step :: inProgressSteps)

        override def next(aid: Atom.Id, count: NonNegShort, step: StepMatch[D]): Builder[D] =
          if (aid === inProgressAtomId) addStep(step)    // continue existing atom
          else InProgress(aid, count, List(step), build) // start a new atom

        override def build: AtomMap[D] =
          if (inProgressSteps.sizeIs != inProgressCount.value) completed // discard in progress atom
          else completed.increment(inProgressSteps.reverse)              // finish in progress atom

    }

  }

  // SequenceMatch is a completed atom map for a sequence along with an id base.
  // The id base is folded into the atom and step UUIDs that will be generated
  // for that sequence.  When this number changes, future atom and step ids will
  // also change.  This is important for acquisition sequences where the same
  // steps are repeatedly executed.
  case class SequenceMatch[D](idBase: Int, atomMap: AtomMap[D])

  object SequenceMatch {

    def Empty[D]: SequenceMatch[D] = SequenceMatch(0, AtomMap.Empty)

    case class Builder[D](idBase: Int, atomMap: AtomMap.Builder[D], resetAtomMap: AtomMap.Builder[D] => AtomMap.Builder[D]):
      def reset = Builder(idBase + 1, resetAtomMap(atomMap), resetAtomMap)
      def build = SequenceMatch(idBase, atomMap.build)

      // Handle the next acquisition step.
      def next(aid: Atom.Id, count: NonNegShort, step: StepMatch[D]): Builder[D] =
        Builder(idBase, atomMap.next(aid, count, step), resetAtomMap)

    object Builder:
      // Acquisition sequence matching starts over from scratch when reset.
      def acq[D]: Builder[D] = Builder(0, AtomMap.Builder.init[D], _ => AtomMap.Builder.init[D])

      // Science sequence matching keeps any progress that has been made when reset.
      def sci[D]: Builder[D] = Builder(0, AtomMap.Builder.init[D], _.reset)
  }

  // The overall completion state for the observation, which consists of the
  // completion state for the acquisition and science sequences.
  case class State[D](acq: SequenceMatch[D], sci: SequenceMatch[D])

  object State {

    def Empty[D]: State[D] = State(SequenceMatch.Empty, SequenceMatch.Empty)

    // MatchContext is the combination of the visit and sequence type.  When
    // either changes, we need to reset the sequence matchers.
    case class MatchContext(vid: Option[Visit.Id], seqType: SequenceType)

    object MatchContext:
      val init = MatchContext(none, SequenceType.Acquisition)

      def fromVisitId(vid: Visit.Id): MatchContext =
        MatchContext(vid.some, SequenceType.Acquisition)

      given Eq[MatchContext] = Eq.fromUniversalEquals

    // The main state builder.  It keeps up with the context (to know when a
    // reset is needed) and with the acquisition and science matchers.
    case class Builder[D](
      ctx: MatchContext,
      acq: SequenceMatch.Builder[D],
      sci: SequenceMatch.Builder[D]
    ) {

      def reset: Builder[D] = Builder(ctx, acq.reset, sci.reset)

      // Handle a new visit apart from any completed steps.  This can result in
      // a context update which causes the sequence matchers to be reset.
      def nextVisit(vid: Visit.Id): Builder[D] =
        val ctxʹ = MatchContext.fromVisitId(vid)
        if (ctxʹ === ctx) this else Builder(ctxʹ, acq.reset, sci.reset)

      // Handle the next completed step, updating the appropriate AtomMap for
      // the sequence type.
      def nextStep(
        vid:     Visit.Id,
        seqType: SequenceType,
        aid:     Atom.Id,
        count:   NonNegShort,
        step:    StepMatch[D]
      ): Builder[D] = {
        val ctxʹ = MatchContext(vid.some, seqType)

        // If it is a new visit, or if we switch from sci to acq or vice versa
        // we reset the acquisition sequence to execute from the beginning.
        val acqʹ = if (ctxʹ === ctx) acq else acq.reset

        // If it is a new visit, we abandon any atom we were working on and
        // start over with a new ids.
        val sciʹ = if (ctxʹ.vid === ctx.vid) sci else sci.reset

        val (acqʹʹ, sciʹʹ) = seqType match {
          case SequenceType.Acquisition => (acqʹ.next(aid, count, step), sciʹ)
          case SequenceType.Science     => (acqʹ, sciʹ.next(aid, count, step))
        }

        Builder(ctxʹ, acqʹʹ, sciʹʹ)
      }

      def build: State[D] = State(acq.build, sci.build)
    }

    object Builder:
      def init[D]: Builder[D] =
        Builder(MatchContext.init, SequenceMatch.Builder.acq, SequenceMatch.Builder.sci)

  }
}
// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Eq
import cats.data.State
import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.SequenceType
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepConfig
import monocle.Focus
import monocle.Lens

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

    def combine[D](ms: Iterable[AtomMap[D]]): AtomMap[D] =
      ms.fold(Empty[D])(_ |+| _)

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

      def toList: List[(AtomMatch[D], PosInt)] =
        m.toList

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
  // also change.
  case class SequenceMatch[D](
    idBase:  Int,
    current: Option[(Visit.Id, AtomMap[D])],
    past:    Map[Visit.Id, AtomMap[D]]
  ) {
    def combinedAtomMap: AtomMap[D] =
      AtomMap.combine(past.values ++ current.map(_._2))
  }

  object SequenceMatch {

    def Empty[D]: SequenceMatch[D] = SequenceMatch(0, none, Map.empty)

    def idBase[D]: Lens[SequenceMatch[D], Int] =
      Focus[SequenceMatch[D]](_.idBase)

    def current[D]: Lens[SequenceMatch[D], Option[(Visit.Id, AtomMap[D])]] =
      Focus[SequenceMatch[D]](_.current)

    def past[D]: Lens[SequenceMatch[D], Map[Visit.Id, AtomMap[D]]] =
      Focus[SequenceMatch[D]](_.past)


    import lucuma.odb.sequence.data.Completion.AtomMap.matchAtom

    def matchCurrent[D](a: ProtoAtom[ProtoStep[D]]): State[SequenceMatch[D], Option[Visit.Id]] =
      State { sm =>
        sm.current.fold((sm, none)) { case (vid, am) =>
          val (amʹ, matches) = am.matchAtom(a)
          (current.replace((vid, amʹ).some)(sm), Option.when(matches)(vid))
        }
      }

    def matchPast[D](a: ProtoAtom[ProtoStep[D]]): State[SequenceMatch[D], Option[Visit.Id]] =
      State { sm =>
        sm.past.foldLeft(none[(SequenceMatch[D], Option[Visit.Id])]) { case (m, (vid, am)) =>
          m orElse {
            val (amʹ, matches) = am.matchAtom(a)
            Option.when(matches)((past.replace(sm.past.updated(vid, amʹ))(sm), vid.some))
          }
        }.getOrElse((sm, none))
      }

    def matchAny[D](a: ProtoAtom[ProtoStep[D]]): State[SequenceMatch[D], Option[Visit.Id]] =
      for {
        m  <- matchPast(a)
        mʹ <- if (m.isDefined) State.pure(m) else matchCurrent(a)
      } yield mʹ

    def atomMap[D](v: Visit.Id): State[SequenceMatch[D], Option[AtomMap[D]]] =
      State.inspect { sm =>
        current.get(sm).filter(_._1 === v).map(_._2) orElse past.get(sm).get(v)
      }

    def contains[D](a: ProtoAtom[ProtoStep[D]])(v: Visit.Id): State[SequenceMatch[D], Boolean] =
      atomMap(v).map(_.exists(_.contains(AtomMatch.fromProtoAtom(a))))

    trait Builder[D, T <: Builder[D, T]] {
      def idBase: Int
      def lastVisit: Option[Visit.Id]
      def visitMap: Map[Visit.Id, AtomMap.Builder[D]]
      def updated(idBase: Int, lastVisit: Option[Visit.Id], visitMap: Map[Visit.Id, AtomMap.Builder[D]]): T
      def reset(vid: Visit.Id): T

      def build: SequenceMatch[D] =
        SequenceMatch(
          idBase,
          lastVisit.flatMap(vid => visitMap.get(vid).map(_.build).tupleLeft(vid)),
          lastVisit.fold(visitMap)(visitMap.removed).view.mapValues(_.build).toMap
        )

      def next(vid: Visit.Id, aid: Atom.Id, count: NonNegShort, step: StepMatch[D]): T =
        updated(idBase, vid.some, visitMap.updatedWith(vid)(_.getOrElse(AtomMap.Builder.init[D]).next(aid, count, step).some))
    }

    // Acquisition sequence matching starts over from scratch when reset.
    case class Acquisition[D](
      idBase:    Int,
      lastVisit: Option[Visit.Id],
      visitMap:  Map[Visit.Id, AtomMap.Builder[D]]
    ) extends Builder[D, Acquisition[D]]:
      def reset(vid: Visit.Id): Acquisition[D] =
         Acquisition(idBase + 1, vid.some, Map(vid -> AtomMap.Builder.init[D]))

      def updated(idBase: Int, lastVisit: Option[Visit.Id], visitMap: Map[Visit.Id, AtomMap.Builder[D]]): Acquisition[D] =
        Acquisition(idBase, lastVisit, visitMap)


    // Science sequence matching keeps any progress that has been made when reset.
    case class Science[D](
      idBase:    Int,
      lastVisit: Option[Visit.Id],
      visitMap:  Map[Visit.Id, AtomMap.Builder[D]]
    ) extends Builder[D, Science[D]]:

      def reset(vid: Visit.Id): Science[D] =
        Science(idBase + 1, vid.some, visitMap.updatedWith(vid)(_.fold(AtomMap.Builder.init[D].some)(_.reset.some)))

      def updated(idBase: Int, lastVisit: Option[Visit.Id], visitMap: Map[Visit.Id, AtomMap.Builder[D]]): Science[D] =
        Science(idBase, lastVisit, visitMap)

    def acq[D]: Acquisition[D] = Acquisition(0, none, Map.empty)
    def sci[D]: Science[D]     = Science(0, none, Map.empty)
  }

  // The overall completion state for the observation, which consists of the
  // completion state for the acquisition and science sequences.
  case class Matcher[D](acq: SequenceMatch[D], sci: SequenceMatch[D])

  object Matcher {

    def Empty[D]: Matcher[D] = Matcher(SequenceMatch.Empty, SequenceMatch.Empty)

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
      acq: SequenceMatch.Acquisition[D],
      sci: SequenceMatch.Science[D]
    ) {

      def reset(vid: Visit.Id): Builder[D] = Builder(ctx, acq.reset(vid), sci.reset(vid))

      // Handle a new visit apart from any completed steps.  This can result in
      // a context update which causes the sequence matchers to be reset.
      def nextVisit(vid: Visit.Id): Builder[D] =
        val ctxʹ = MatchContext.fromVisitId(vid)
        if (ctxʹ === ctx) this else Builder(ctxʹ, acq.reset(vid), sci.reset(vid))

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
        val acqʹ = if (ctxʹ === ctx) acq else acq.reset(vid)

        // If it is a new visit, we abandon any atom we were working on and
        // start over with a new ids.
        val sciʹ = if (ctxʹ.vid === ctx.vid) sci else sci.reset(vid)

        val (acqʹʹ, sciʹʹ) = seqType match {
          case SequenceType.Acquisition => (acqʹ.next(vid, aid, count, step), sciʹ)
          case SequenceType.Science     => (acqʹ, sciʹ.next(vid, aid, count, step))
        }

        Builder(ctxʹ, acqʹʹ, sciʹʹ)
      }

      def build: Matcher[D] = Matcher(acq.build, sci.build)
    }

    object Builder:
      def init[D]: Builder[D] =
        Builder(MatchContext.init, SequenceMatch.acq, SequenceMatch.sci)

  }
}
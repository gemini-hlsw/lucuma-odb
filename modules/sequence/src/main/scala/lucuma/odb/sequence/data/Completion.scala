// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.SequenceType
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepConfig

object Completion {

  type StepMatch[D] = (D, StepConfig)
  type AtomMatch[D] = List[StepMatch[D]]

  object AtomMatch {
    def fromProtoAtom[D](atom: ProtoAtom[ProtoStep[D]]): AtomMatch[D] =
      atom.steps.map(s => (s.value, s.stepConfig)).toList
  }

  /**
   * AtomMap associates atom configurations with a count of executed instances
   * of the atom.  This is used to filter executed atoms when producing the
   * sequence.
   *
   * @tparam D dynamic instrument configuration type
   */
  opaque type AtomMap[D] = Map[AtomMatch[D], PosInt]

  object AtomMap {

    def Empty[D]: AtomMap[D] =
      Map.empty

    def from[D](seq: (AtomMatch[D], PosInt)*): AtomMap[D] =
      Map.from(seq)

    extension [D](m: AtomMap[D]) {

      def increment(k: AtomMatch[D]): AtomMap[D] =
        m.updatedWith(k) {
          case None    => PosInt.unsafeFrom(1).some
          case Some(p) => PosInt.from(p.value + 1).toOption
        }

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
      def matchAtom(
        a: ProtoAtom[ProtoStep[D]]
      ): (AtomMap[D], Boolean) = {
        val k = AtomMatch.fromProtoAtom(a)
        if (contains(k)) (decrement(k), true) else (m, false)
      }

    }

    trait Builder[D] {
      def reset: Builder[D]
      def next(aid: Atom.Id, count: NonNegShort, step: StepMatch[D]): Builder[D]
      def build: AtomMap[D]
    }

    object Builder {

      def init[D]: Builder[D] = Reset(AtomMap.Empty)

      private case class Reset[D](completed: AtomMap[D]) extends Builder[D] {
        override def reset: Builder[D] = this

        override def next(aid: Atom.Id, count: NonNegShort, step: StepMatch[D]): Builder[D] =
          InProgress(aid, count, List(step), completed)

        override def build: AtomMap[D] = completed
      }

      private case class InProgress[D](
        inProgressAtomId: Atom.Id,
        inProgressCount:  NonNegShort,
        inProgressSteps:  AtomMatch[D],
        completed:        AtomMap[D]
      ) extends Builder[D] {

        override def reset: Builder[D] = Reset(build)

        private def addStep(step: StepMatch[D]): InProgress[D] =
          copy(inProgressSteps = step :: inProgressSteps)

        override def next(aid: Atom.Id, count: NonNegShort, step: StepMatch[D]): Builder[D] =
          if (aid === inProgressAtomId) addStep(step)    // continue existing atom
          else InProgress(aid, count, List(step), build) // start a new atom

        override def build: AtomMap[D] =
          if (inProgressSteps.sizeIs != inProgressCount.value) completed
          else completed.increment(inProgressSteps.reverse)

      }

    }

  }

  case class Sequence[D](
    cycleCount: Int,
    atomMap:    AtomMap[D]
  )

  case class State[D](
    acq: Sequence[D],
    sci: Sequence[D]
  )

  object State {

    case class Builder[D](
      previousVisit: Option[Visit.Id],
      previousType:  Option[SequenceType],
      acqCycles:     Int,
      acqBuild:      AtomMap.Builder[D],
      sciCycles:     Int,
      sciBuild:      AtomMap.Builder[D]
    ) {

      def reset: Builder[D] =
        Builder(
          previousVisit,
          previousType,
          acqCycles,
          AtomMap.Builder.init[D],
          sciCycles,
          sciBuild.reset
        )

      def next(
        vid:     Visit.Id,
        seqType: SequenceType,
        aid:     Atom.Id,
        count:   NonNegShort,
        step:    StepMatch[D]
      ): Builder[D] = {
        val mk: (Int, AtomMap.Builder[D], Int, AtomMap.Builder[D]) => Builder[D] =
          Builder(vid.some, seqType.some, _, _, _, _)

        (seqType, previousVisit.forall(_ =!= vid) || previousType.forall(_ =!= seqType)) match {
          case (SequenceType.Acquisition, true ) =>
            mk(acqCycles + 1, AtomMap.Builder.init[D].next(aid, count, step), sciCycles, sciBuild)

          case (SequenceType.Acquisition, false) =>
            mk(acqCycles, acqBuild.next(aid, count, step), sciCycles, sciBuild)

          case (SequenceType.Science,     true ) =>
            mk(acqCycles, AtomMap.Builder.init[D], sciCycles + 1, sciBuild.reset.next(aid, count, step))

          case (SequenceType.Science,     false) =>
            mk(acqCycles, AtomMap.Builder.init[D], sciCycles, sciBuild.next(aid, count, step))
        }
      }

      def build: State[D] =
        State(Sequence(acqCycles, acqBuild.build), Sequence(sciCycles, sciBuild.build))
    }

    object Builder {
      def init[D]: Builder[D] =
        Builder(None, None, 0, AtomMap.Builder.init[D], 0, AtomMap.Builder.init[D])
    }

  }


}

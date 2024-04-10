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
    idBase:  Int,
    atomMap: AtomMap[D]
  )

  object Sequence {

    def idBase[D](b: Int): Sequence[D] =
      Sequence(b, AtomMap.Empty)

    def Empty[D]: Sequence[D] =
      idBase(0)

  }

  case class State[D](
    acq: Sequence[D],
    sci: Sequence[D]
  )

  object State {

    def idBase[D](b: Int): State[D] =
      State(Sequence.idBase(b), Sequence.idBase(b))

    def Empty[D]: State[D] =
      idBase(0)

    case class Builder[D](
      previousVisit: Option[Visit.Id],
      previousType:  Option[SequenceType],
      acqIdBase:     Int,
      acqBuild:      AtomMap.Builder[D],
      sciIdBase:     Int,
      sciBuild:      AtomMap.Builder[D]
    ) {

      def reset: Builder[D] =
        Builder(
          previousVisit,
          previousType,
          acqIdBase,
          AtomMap.Builder.init[D],
          sciIdBase,
          sciBuild.reset
        )

      def nextVisit(
        vid: Visit.Id
      ): Builder[D] =
        if (previousVisit.exists(_ === vid))
          this
        else
          Builder(
            vid.some,
            previousType,
            acqIdBase + 1,
            AtomMap.Builder.init[D],
            sciIdBase + 1,
            sciBuild.reset
          )

      def nextStep(
        vid:     Visit.Id,
        seqType: SequenceType,
        aid:     Atom.Id,
        count:   NonNegShort,
        step:    StepMatch[D]
      ): Builder[D] = {
        val isNewBase = previousVisit.forall(_ =!= vid) || previousType.forall(_ =!= seqType)

        val (acqIdBaseʹ, sciIdBaseʹ) = (previousType, isNewBase) match {
          case (Some(SequenceType.Acquisition), true) => (acqIdBase + 1, sciIdBase)
          case (Some(SequenceType.Science),     true) => (acqIdBase, sciIdBase + 1)
          case _                                      => (acqIdBase, sciIdBase)
        }

        val (acqBuildʹ, sciBuildʹ) = (seqType, isNewBase) match {
          case (SequenceType.Acquisition, true ) => (AtomMap.Builder.init[D].next(aid, count, step), sciBuild)
          case (SequenceType.Acquisition, false) => (acqBuild.next(aid, count, step), sciBuild)
          case (SequenceType.Science,     true ) => (AtomMap.Builder.init[D], sciBuild.reset.next(aid, count, step))
          case (SequenceType.Science,     false) => (AtomMap.Builder.init[D], sciBuild.next(aid, count, step))
        }

        Builder(vid.some, seqType.some, acqIdBaseʹ, acqBuildʹ, sciIdBaseʹ, sciBuildʹ)
      }

      def build: State[D] =
        State(Sequence(acqIdBase, acqBuild.build), Sequence(sciIdBase, sciBuild.build))
    }

    object Builder {
      def init[D]: Builder[D] =
        Builder(None, None, 0, AtomMap.Builder.init[D], 0, AtomMap.Builder.init[D])
    }

  }


}

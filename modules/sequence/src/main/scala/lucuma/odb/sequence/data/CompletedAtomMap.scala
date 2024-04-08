// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepConfig

/**
 * CompletedAtomMap associates atom configurations with a count of executed
 * instances of the atom.  This is used to filter executed atoms when producing
 * the sequence.
 *
 * @tparam D dynamic instrument configuration type
 */
opaque type CompletedAtomMap[D] = Map[CompletedAtomMap.AtomMatch[D], PosInt]

object CompletedAtomMap {

  type StepMatch[D] = (D, StepConfig)
  type AtomMatch[D] = List[StepMatch[D]]

  object AtomMatch {
    def fromProtoAtom[D](atom: ProtoAtom[ProtoStep[D]]): AtomMatch[D] =
      atom.steps.map(s => (s.value, s.stepConfig)).toList
  }

  def Empty[D]: CompletedAtomMap[D] =
    Map.empty

  def from[D](seq: (AtomMatch[D], PosInt)*): CompletedAtomMap[D] =
    Map.from(seq)

  extension [D](m: CompletedAtomMap[D]) {

    def increment(k: AtomMatch[D]): CompletedAtomMap[D] =
      m.updatedWith(k) {
        case None    => PosInt.unsafeFrom(1).some
        case Some(p) => PosInt.from(p.value + 1).toOption
      }
      
    def decrement(k: AtomMatch[D]): CompletedAtomMap[D] =
      m.updatedWith(k)(_.flatMap(p => PosInt.from(p.value - 1).toOption))

    def contains(k: AtomMatch[D]): Boolean =
      m.contains(k)

    def isEmpty: Boolean =
      m.isEmpty

    def toMap: Map[AtomMatch[D], PosInt] =
      m

    /**
     * Match the given `ProtoAtom[ProtoStep[D]]` against the completed atoms.
     * If there is at least one matching completed atom, `true` is returned and
     * a copy of the completed atom map with one fewer instance.  Otherwise
     * `false` is returned along with this completed atom map.
     */
    def matchAtom(
      a: ProtoAtom[ProtoStep[D]]
    ): (CompletedAtomMap[D], Boolean) = {
      val k = AtomMatch.fromProtoAtom(a)
      if (contains(k)) (decrement(k), true) else (m, false)
    }

  }

  trait Builder[D] {
    def reset: Builder[D]
    def next(aid: Atom.Id, count: NonNegShort, step: StepMatch[D]): Builder[D]
    def build: CompletedAtomMap[D]
  }

  object Builder {

    def init[D]: Builder[D] = Reset(CompletedAtomMap.Empty)

    private case class Reset[D](completed: CompletedAtomMap[D]) extends Builder[D] {
      override def reset: Builder[D] = this

      override def next(aid: Atom.Id, count: NonNegShort, step: StepMatch[D]): Builder[D] =
        InProgress(aid, count, List(step), completed)

      override def build: CompletedAtomMap[D] = completed
    }

    private case class InProgress[D](
      inProgressAtomId: Atom.Id,
      inProgressCount:  NonNegShort,
      inProgressSteps:  AtomMatch[D],
      completed:        CompletedAtomMap[D]
    ) extends Builder[D] {

      override def reset: Builder[D] = Reset(completed)

      private def addStep(step: StepMatch[D]): InProgress[D] =
        copy(inProgressSteps = step :: inProgressSteps)

      override def next(aid: Atom.Id, count: NonNegShort, step: StepMatch[D]): Builder[D] =
        if (aid === inProgressAtomId) addStep(step)    // continue existing atom
        else InProgress(aid, count, List(step), build) // start a new atom

      private def inProgressKey: AtomMatch[D] = inProgressSteps.reverse

      override def build: CompletedAtomMap[D] =
        if (inProgressSteps.sizeIs != inProgressCount.value) completed
        else completed.increment(inProgressKey)

    }

  }

}
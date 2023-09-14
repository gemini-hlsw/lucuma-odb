// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.StepConfig


opaque type CompletedAtomMap[D] = Map[CompletedAtomMap.Key[D], PosInt]

object CompletedAtomMap {

  type StepMatch[D] = (D, StepConfig)
  type AtomMatch[D] = List[StepMatch[D]]

  case class Key[D](
    sequenceType: SequenceType,
    atomMatch:    AtomMatch[D]
  )

  object Key {

    def acquisition[D](steps: StepMatch[D]*): Key[D] =
      Key(SequenceType.Acquisition, steps.toList)

    def science[D](steps: StepMatch[D]*): Key[D] =
      Key(SequenceType.Science, steps.toList)

  }

  def Empty[D]: CompletedAtomMap[D] =
    Map.empty

  def from[D](seq: (Key[D], PosInt)*): CompletedAtomMap[D] =
    Map.from(seq)

  extension [D](m: CompletedAtomMap[D]) {

    def increment(k: Key[D]): CompletedAtomMap[D] =
      m.updatedWith(k) {
        case None    => PosInt.unsafeFrom(1).some
        case Some(p) => PosInt.from(p.value + 1).toOption
      }

    def decrement(k: Key[D]): CompletedAtomMap[D] =
      m.updatedWith(k)(_.flatMap(p => PosInt.from(p.value - 1).toOption))

    def contains(k: Key[D]): Boolean =
      m.contains(k)

    def isEmpty: Boolean =
      m.isEmpty

    def toMap: Map[Key[D], PosInt] =
      m

  }

}


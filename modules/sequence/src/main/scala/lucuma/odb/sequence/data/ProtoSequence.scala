// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.data.NonEmptyList
import monocle.Focus
import monocle.Lens

final case class ProtoSequence[D](
  atoms: NonEmptyList[ProtoAtom[D]]
)

object ProtoSequence {

  def one[D](a: ProtoAtom[D]): ProtoSequence[D] =
    ProtoSequence(NonEmptyList.one(a))

  def of[D](a: ProtoAtom[D], as: ProtoAtom[D]*): ProtoSequence[D] =
    ProtoSequence(NonEmptyList.of(a, as*))

  def atoms[D]: Lens[ProtoSequence[D], NonEmptyList[ProtoAtom[D]]] =
    Focus[ProtoSequence[D]](_.atoms)

}

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.data.NonEmptyList
import monocle.Focus
import monocle.Lens

/**
 * An atom (list of steps) without an id.
 */
final case class ProtoAtom[D](
  steps: NonEmptyList[ProtoStep[D]]
)

object ProtoAtom {

  def one[D](p: ProtoStep[D]): ProtoAtom[D] =
    ProtoAtom(NonEmptyList.one(p))

  def of[D](p: ProtoStep[D], ps: ProtoStep[D]*): ProtoAtom[D] =
    ProtoAtom(NonEmptyList.of(p, ps*))

  def steps[D]: Lens[ProtoAtom[D], NonEmptyList[ProtoStep[D]]] =
    Focus[ProtoAtom[D]](_.steps)

}



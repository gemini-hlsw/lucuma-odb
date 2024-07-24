// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.longslit

import cats.data.NonEmptyList
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.sequence.data.ProtoStep

/**
 * Science and associated matching flat.
 */
final case class ScienceAtom[D](
  description: NonEmptyString,
  stepOrder:   StepOrder,
  science:     ProtoStep[D],
  flat:        ProtoStep[D]
) {

  def steps: NonEmptyList[ProtoStep[D]] =
    stepOrder match {
      case StepOrder.ScienceThenFlat => NonEmptyList.of(science, flat)
      case StepOrder.FlatThenScience => NonEmptyList.of(flat, science)
    }

}

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.syntax.all.*
import lucuma.core.enums.ScienceBand
import lucuma.core.model.ObservationValidation
import lucuma.core.syntax.string.*
import lucuma.odb.data.ObservationValidationMap

object BandValidator extends ObservationValidator:

  def invalidScienceBand(b: ScienceBand): String =
    s"Science Band ${b.tag.toScreamingSnakeCase} has no time allocation."

  val missingScienceBand: String =
    "Please select a science band."

  // Only programs with allocations can carry a band.
  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    info.programAllocations.foldMap: bs =>
      info.scienceBand match
        case None                             => ObservationValidationMap.singleton(ObservationValidation.configuration(missingScienceBand))
        case Some(b) if bs.toList.contains(b) => ObservationValidationMap.empty
        case Some(b)                          => ObservationValidationMap.singleton(ObservationValidation.configuration(invalidScienceBand(b)))

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Wavelength

/** Observing constraints, used to narrow the space of compatible observing modes. */
sealed trait Constraints

object Constraints {

  /** Observing constraints for spectroscopy. */
  final case class Spectroscopy(
    λ:                    Wavelength,
    simultaneousCoverage: Wavelength,
    resolution:           PosInt // todo: Resolution
  ) extends Constraints

}

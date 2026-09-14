// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import lucuma.core.enums.FieldLens
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue

/**
 * Altair adaptive optics parameters as the ITC needs them, mirroring the OCS ITC form: the natural
 * guide star's separation from the science target and its R-band brightness drive the Strehl
 * model. Not tied to an instrument; GNIRS is the only user for now.
 */
sealed trait AltairParameters derives Eq

object AltairParameters:

  /** Natural guide star mode. The field lens is the user's choice. */
  case class Ngs(
    guideStarSeparation: Angle,
    guideStarBrightness: BrightnessValue,
    fieldLens:           FieldLens
  ) extends AltairParameters derives Eq

  /** Laser guide star with the tip/tilt star on the Altair WFS. The field lens is always in. */
  case class Lgs(
    guideStarSeparation: Angle,
    guideStarBrightness: BrightnessValue
  ) extends AltairParameters derives Eq

  /**
   * Laser guide star with the tip/tilt star on PWFS1. The legacy ITC has no model for it: the
   * correction is modest and independent of the guide star, so it is computed without Altair at
   * the 20% image quality bin. No parameters.
   */
  case object LgsP1 extends AltairParameters


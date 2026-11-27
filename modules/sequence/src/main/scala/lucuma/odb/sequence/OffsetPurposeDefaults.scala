// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.OffsetPurpose
import lucuma.core.enums.ScorpioFpu
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.flamingos2.scienceArea as Flamingos2ScienceArea
import lucuma.core.geom.gnirs.scienceArea as GnirsScienceArea
import lucuma.core.geom.igrins2.scienceArea as Igrins2ScienceArea
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.scorpio.scienceArea as ScorpioScienceArea
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.gnirs.GnirsFpu

/**
 * The purpose a telescope position has when the user leaves it unspecified,
 * per observing mode.  Where the mode has a science aperture with a known shape,
 * a position is `Object` when the target falls inside it and `Sky` otherwise.
 * Modes without a sky concept treat every position as `Object`.
 */
object OffsetPurposeDefaults:

  /** Derives the purpose of a position from its offset. */
  type Resolver = Offset => OffsetPurpose

  /** Every position is on the object. */
  val AlwaysObject: Resolver =
    _ => OffsetPurpose.Object

  private def inside(scienceArea: ShapeExpression): Resolver =
    OffsetPurpose.fromScienceArea(scienceArea)

  /** IGRINS-2 long slit: inside the 0.3" x 5" slit. */
  val igrins2LongSlit: Resolver =
    inside(Igrins2ScienceArea.scienceSlitFOV)

  /** IGRINS-2 slit-viewing camera images: inside the 46" SVC field. */
  val igrins2Svc: Resolver =
    inside(Igrins2ScienceArea.svcFieldOfView)

  /** GNIRS spectroscopy: inside the slit (which depends on camera and prism) or the IFU field. */
  def gnirsSpectroscopy(
    fpu:    GnirsFpu.Spectroscopy,
    camera: GnirsCamera,
    prism:  GnirsPrism
  ): Resolver =
    GnirsScienceArea
      .shapeAt(Angle.Angle0, Offset.Zero, fpu, camera, prism)
      .fold(AlwaysObject)(inside)

  /**
   * Flamingos 2 spectroscopy: inside the long slit.  For MOS this is the builtin
   * long slit equivalent to the mask, an approximation of the individual slits.
   */
  def flamingos2Spectroscopy(equivalentFpu: Flamingos2Fpu): Resolver =
    inside(
      Flamingos2ScienceArea.shapeAt(
        Angle.Angle0,
        Offset.Zero,
        Flamingos2LyotWheel.F16,
        Flamingos2FpuMask.Builtin(equivalentFpu)
      )
    )

  /** SCORPIO long slit: inside the slit. */
  def scorpioLongSlit(fpu: ScorpioFpu): Resolver =
    inside(ScorpioScienceArea.longSlitFov(fpu.slitWidth))

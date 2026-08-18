// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.derived.*
import cats.syntax.eq.*
import coulomb.integrations.cats.quantity.given
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Instrument
import lucuma.core.enums.MosDispersionDirection
import lucuma.core.enums.MosSlitPriority
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Redshift
import lucuma.core.math.units.PixelScale
import lucuma.core.model.mos.MosMaskHeader
import lucuma.core.model.mos.MosMaskProvenance
import lucuma.core.model.mos.MosMaskSlit
import lucuma.core.model.mos.MosObjectId

/**
 * One aperture of a MOS mask design, trimmed to what is recorded on the
 * attachment.
 *
 * The two offsets and the tilt are signed quantities held in a type that
 * wraps at a full turn, so they must be read through
 * `Angle.signedMicroarcseconds` (or an equivalent signed optic) rather than
 * as plain magnitudes.
 *
 * The magnitude and redshift are stored on the attachment but not yet
 * exposed in the GraphQL schema; exposing them later is a schema change with
 * no re-upload of existing masks.
 */
case class MaskSlit(
  id:               MosObjectId,
  coordinates:      Coordinates,
  x:                BigDecimal,
  y:                BigDecimal,
  width:            Angle,
  length:           Angle,
  offsetAlongSlit:  Angle,
  offsetAcrossSlit: Angle,
  tilt:             Angle,
  priority:         MosSlitPriority,
  magnitude:        BrightnessValue,
  redshift:         Option[Redshift]
) derives Eq

object MaskSlit:

  def fromMosMaskSlit(slit: MosMaskSlit): MaskSlit =
    MaskSlit(
      id               = slit.id,
      coordinates      = slit.coordinates,
      x                = BigDecimal(slit.x.toString()),
      y                = BigDecimal(slit.y.toString()),
      width            = slit.slitWidth,
      length           = slit.slitLength,
      offsetAlongSlit  = slit.offsetAlongSlit,
      offsetAcrossSlit = slit.offsetAcrossSlit,
      tilt             = slit.tilt,
      priority         = slit.priority,
      magnitude        = slit.magnitude,
      redshift         = slit.redshift
    )

/**
 * The design read from a MOS mask attachment's file at upload.
 *
 * The dispersion direction, tilted-slit flag and provenance are stored on
 * the attachment but not yet exposed in the GraphQL schema; exposing them
 * later is a schema change with no re-upload of existing masks.
 */
case class MaskDefinition(
  name:                NonEmptyString,
  instrument:          Instrument,
  pixelScale:          PixelScale,
  pointing:            Coordinates,
  positionAngle:       Angle,
  dispersionDirection: MosDispersionDirection,
  hasTiltedSlits:      Boolean,
  provenance:          MosMaskProvenance,
  slits:               List[MaskSlit]
) derives Eq:

  /** Slits that place science objects, excluding alignment-star boxes. */
  def scienceSlits: List[MaskSlit] =
    slits.filter(_.priority =!= MosSlitPriority.Acquisition)

  /** Alignment-star boxes used to position the mask on sky. */
  def acquisitionSlits: List[MaskSlit] =
    slits.filter(_.priority === MosSlitPriority.Acquisition)

  /**
   * Mean width of the science slits, or None for a design with none.
   * Alignment-star boxes are excluded so their wide boxes do not skew the
   * mean.
   */
  def averageSlitWidth: Option[Angle] =
    val sci = scienceSlits
    Option.when(sci.nonEmpty):
      Angle.fromMicroarcseconds(sci.map(_.width.toMicroarcseconds).sum / sci.length)

object MaskDefinition:

  /**
   * The design, or None when it records no position angle.  A mask cannot be
   * observed without knowing the angle to observe it at, so such a design is
   * not accepted.
   */
  def fromMosMask(
    name:   NonEmptyString,
    header: MosMaskHeader,
    slits:  List[MosMaskSlit]
  ): Option[MaskDefinition] =
    header.positionAngle.map: pa =>
      MaskDefinition(
        name                = name,
        instrument          = header.instrument,
        pixelScale          = header.pixelScale,
        pointing            = header.pointing,
        positionAngle       = pa,
        dispersionDirection = header.dispersionDirection,
        hasTiltedSlits      = header.hasTiltedSlits,
        provenance          = header.provenance,
        slits               = slits.map(MaskSlit.fromMosMaskSlit)
      )

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.derived.*
import coulomb.integrations.cats.quantity.given
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Instrument
import lucuma.core.enums.MosSlitPriority
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.units.PixelScale
import lucuma.core.model.mos.MosMaskHeader
import lucuma.core.model.mos.MosMaskSlit
import lucuma.core.model.mos.MosObjectId

/**
 * One aperture of a MOS mask design, trimmed to what is recorded on the
 * attachment
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
  priority:         MosSlitPriority
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
      priority         = slit.priority
    )

/**
 * The design read from a MOS mask attachment's file at upload.
 */
case class MaskDefinition(
  name:          NonEmptyString,
  instrument:    Instrument,
  pixelScale:    PixelScale,
  pointing:      Coordinates,
  positionAngle: Option[Angle],
  slits:         List[MaskSlit]
) derives Eq

object MaskDefinition:

  def fromMosMask(
    name:   NonEmptyString,
    header: MosMaskHeader,
    slits:  List[MosMaskSlit]
  ): MaskDefinition =
    MaskDefinition(
      name          = name,
      instrument    = header.instrument,
      pixelScale    = header.pixelScale,
      pointing      = header.pointing,
      positionAngle = header.positionAngle,
      slits         = slits.map(MaskSlit.fromMosMaskSlit)
    )

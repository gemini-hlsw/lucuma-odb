// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Hash
import cats.derived.*
import lucuma.core.enums.Band
import lucuma.core.math.Redshift
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.itc.service.hashes.given
import monocle.Focus
import monocle.Lens

case class TargetData(
  sourceProfile: SourceProfile,
  redshift:      Redshift
) derives Hash:
  def bandOrLine(atWavelength: Wavelength): Either[Band, Wavelength] =
    sourceProfile
      .nearestBand(atWavelength)
      .toLeft:
        sourceProfile
          .nearestLine(atWavelength)
          .getOrElse(throw new RuntimeException("No brightness measures provided for target."))

object TargetData:
  val sourceProfile: Lens[TargetData, SourceProfile] = Focus[TargetData](_.sourceProfile)
  val redshift: Lens[TargetData, Redshift]           = Focus[TargetData](_.redshift)

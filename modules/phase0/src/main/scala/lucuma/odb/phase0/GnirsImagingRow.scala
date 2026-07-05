// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Rfc5234.htab
import cats.syntax.all.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.Instrument
import lucuma.core.math.Wavelength

case class GnirsImagingRow(
  img:    ImagingRow,
  filter: GnirsFilter,
  camera: GnirsCamera
)

object GnirsImagingRow:

  // Keyhole imaging filter set: the small MK filters Y/J/K plus the full-size X and H
  // order-blockers. Short names are unambiguous within this set, unlike in the full
  // enum, where the broadband J/K repeat the order-blocker names.
  val ImagingFilters: NonEmptyList[GnirsFilter] =
    NonEmptyList.of(
      GnirsFilter.Y,
      GnirsFilter.Order6,
      GnirsFilter.J,
      GnirsFilter.Order4,
      GnirsFilter.K
    )

  private val CameraCutoffWavelength: Wavelength = Wavelength.unsafeFromIntPicometers(2_700_000) // 2.7μm is the cutoff between the red and blue cameras.

  // The instrument column carries the camera ("GNIRS SC" or "GNIRS LC"); blue vs red
  // is determined by the filter wavelength, as in GnirsSpectroscopyRow.
  private def cameraFromLabel(label: String, filter: GnirsFilter): Either[String, GnirsCamera] =
    val isBlue: Boolean = filter.centralWavelength < CameraCutoffWavelength
    (label, isBlue) match
      case ("GNIRS SC", true)  => GnirsCamera.ShortBlue.asRight
      case ("GNIRS SC", false) => GnirsCamera.ShortRed.asRight
      case ("GNIRS LC", true)  => GnirsCamera.LongBlue.asRight
      case ("GNIRS LC", false) => GnirsCamera.LongRed.asRight
      case _                   => s"Cannot determine camera from: $label".asLeft

  val gnirs: Parser[List[GnirsImagingRow]] = (
    (RowParsers.string            <* htab) ~
    (RowParsers.arcsec            <* htab) ~
    (ImagingRow.filterOptions     <* htab) ~
    (RowParsers.ao                <* htab) ~
    (ImagingRow.imagingCapability <* htab) ~
    RowParsers.site
  ).flatMap { case (((((label, fov), filterOpts), ao), capability), site) =>
    filterOpts.toList.traverse: f =>
      val row: Either[String, GnirsImagingRow] =
        for
          filter <- ImagingFilters.find(_.shortName === f).toRight(s"Cannot find GNIRS imaging filter: $f.")
          camera <- cameraFromLabel(label, filter)
        yield GnirsImagingRow(ImagingRow(Instrument.Gnirs, fov, f, ao, capability, site), filter, camera)

      row.fold(Parser.failWith, Parser.pure)
  }

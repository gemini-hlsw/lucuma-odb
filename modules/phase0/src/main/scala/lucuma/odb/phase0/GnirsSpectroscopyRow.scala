// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.Parser
import cats.syntax.all.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.Instrument
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

case class GnirsSpectroscopyRow(
  spec:    SpectroscopyRow,
  grating: GnirsGrating,
  filter:  GnirsFilter,
  fpu:     GnirsFpuSlit,
  prism:   GnirsPrism,
  camera:  GnirsCamera
)

object GnirsSpectroscopyRow:

  private val CameraCutoffWavelength = Wavelength.unsafeFromIntPicometers(2_700_000) // 2.7μm is the cutoff between the red and blue cameras.

  private def prismFromDescription(description: String): Either[String, GnirsPrism] =
    description.split("\\s").toList.get(1) match // Format is "<CAMERA>" for Mirror or "<CAMERA> <PRISM>" for XD.
      case Some("XD") | Some("SXD") => GnirsPrism.Sxd.asRight // "SC XD" or "LC SXD"
      case Some("LXD") => GnirsPrism.Lxd.asRight // "LC LXD"
      case None => GnirsPrism.Mirror.asRight
      case _ => s"Cannot determine prism from description: $description".asLeft

  private def cameraFromDescriptionAndWavelength(description: String, wavelength: Wavelength): Either[String, GnirsCamera] =
    val isBlue: Boolean = wavelength < CameraCutoffWavelength
    (description.split("\\s").toList.head, isBlue) match
      case ("SC", true) => GnirsCamera.ShortBlue.asRight
      case ("SC", false) => GnirsCamera.ShortRed.asRight
      case ("LC", true) => GnirsCamera.LongBlue.asRight
      case ("LC", false) => GnirsCamera.LongRed.asRight
      case _ => s"Cannot determine camera from description: $description and wavelength: $wavelength".asLeft

  val gnirs: Parser[List[GnirsSpectroscopyRow]] =
    SpectroscopyRow.rows.flatMap: rs => // Only SingleSlit and XD for now, no IFU
      rs.filter(_.fpuOption === FpuOption.Singleslit).traverse: r =>
        val row = for
          _       <- Either.raiseWhen(r.instrument =!= Instrument.Gnirs)(s"Cannot parse a ${r.instrument.tag} as Gnirs")
          filter  <- r.filter
                       .toRight("GNIRS spectroscopy requires a filter")
                       .flatMap: f =>
                         Enumerated[GnirsFilter]
                           .all
                           .find(_.shortName === f)
                           .toRight(s"Cannot find filter: $f. Does a value exist in the Enumerated?")
          grating <- Enumerated[GnirsGrating]
                        .all
                        .find(_.shortName === r.disperser)
                        .toRight(s"Cannot find grating: ${r.disperser}. Does a value exist in the Enumerated?")
          fpu     <- Enumerated[GnirsFpuSlit]
                        .all
                        .find(_.slitWidth === r.slitWidth)
                        .toRight(s"Cannot find FPU: ${r.fpu}. Does a value exist in the Enumerated?")
          prism   <- prismFromDescription(r.description)
          camera  <- cameraFromDescriptionAndWavelength(r.description, r.wavelengthOpt)
        yield GnirsSpectroscopyRow(r, grating, filter, fpu, prism, camera)

        row.fold(Parser.failWith, Parser.pure)

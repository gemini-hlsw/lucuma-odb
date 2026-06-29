// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.Parser
import cats.syntax.all.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuIfu
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
  fpu:     Either[GnirsFpuSlit, GnirsFpuIfu],
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

  // FPU options imported for GNIRS: single-slit (incl. XD) and IFU, no MOS.
  private val ImportedFpuOptions: Set[FpuOption] =
    Set(FpuOption.Singleslit, FpuOption.Ifu)

  // The camera is determined by the description prefix (camera/resolution) and the
  // optimal wavelength (blue vs red). Single-slit/XD rows start with "SC"/"LC"; IFU
  // rows start with "LR-IFU"/"HR-IFU" (low res → short camera, high res → long camera).
  private def cameraFromDescriptionAndWavelength(description: String, wavelength: Wavelength): Either[String, GnirsCamera] =
    val isBlue: Boolean = wavelength < CameraCutoffWavelength
    (description.split("\\s").toList.head, isBlue) match
      case ("SC" | "LR-IFU", true)  => GnirsCamera.ShortBlue.asRight
      case ("SC" | "LR-IFU", false) => GnirsCamera.ShortRed.asRight
      case ("LC" | "HR-IFU", true)  => GnirsCamera.LongBlue.asRight
      case ("LC" | "HR-IFU", false) => GnirsCamera.LongRed.asRight
      case _ => s"Cannot determine camera from description: $description and wavelength: $wavelength".asLeft

  val gnirs: Parser[List[GnirsSpectroscopyRow]] =
    SpectroscopyRow.rows.flatMap: rs =>
      rs.filter(r => ImportedFpuOptions.contains(r.fpuOption)).traverse: r =>
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
                        .find(_.rulingDensity.toString === r.disperser)
                        .toRight(s"Cannot find grating: ${r.disperser}. Does a value exist in the Enumerated?")
          fpu     <- r.fpuOption match
                       case FpuOption.Ifu =>
                         Enumerated[GnirsFpuIfu]
                           .all
                           .find(_.shortName === r.fpu)
                           .toRight(s"Cannot find IFU FPU: ${r.fpu}. Does a value exist in the Enumerated?")
                           .map(_.asRight)
                       case _ => // Singleslit
                         Enumerated[GnirsFpuSlit]
                           .all
                           .find(_.slitWidth === r.slitWidth)
                           .toRight(s"Cannot find FPU: ${r.fpu}. Does a value exist in the Enumerated?")
                           .map(_.asLeft)
          prism   <- prismFromDescription(r.description)
          camera  <- cameraFromDescriptionAndWavelength(r.description, r.wavelengthOpt)
        yield GnirsSpectroscopyRow(r, grating, filter, fpu, prism, camera)

        row.fold(Parser.failWith, Parser.pure)

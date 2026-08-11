// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.*

/**
 * One GNIRS spectroscopy science configuration: a central wavelength with the
 * exposure time mode and coadds that apply there.  A missing exposure time mode
 * falls back to the observation's requirements; missing coadds default to 1.
 */
case class GnirsCentralWavelengthConfigInput(
  centralWavelength: Wavelength,
  exposureTimeMode:  Option[ExposureTimeMode],
  coadds:            Option[PosInt]
)

object GnirsCentralWavelengthConfigInput:

  val Binding: Matcher[GnirsCentralWavelengthConfigInput] =
    ObjectFieldsBinding.rmap:
      case List(
        WavelengthInput.Binding("centralWavelength", rCentralWavelength),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm),
        PosIntBinding.Option("coadds", rCoadds)
      ) =>
        (rCentralWavelength, rEtm, rCoadds).parMapN: (w, etm, coadds) =>
          GnirsCentralWavelengthConfigInput(w, etm, GnirsCentralWavelengthConfigInput.coaddsForEtm(etm, coadds))

  /**
   * Signal-to-noise exposure time mode does not support coadds.  When the ETM is
   * set to signal-to-noise, force coadds to 1 so a previously-set value doesn't
   * linger.
   */
  private def coaddsForEtm(
    etm:    Option[ExposureTimeMode],
    coadds: Option[PosInt]
  ): Option[PosInt] =
    etm match
      case Some(ExposureTimeMode.SignalToNoiseMode(_, _)) => PosInt.from(1).toOption
      case _                                              => coadds

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs.spectroscopy

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

/**
 * One GNIRS spectroscopy science configuration: a central wavelength together
 * with the exposure time mode and coadds that apply at that wavelength.  Each is
 * a separate ITC calculation and a separate block of science steps with its own
 * flats and arcs.
 */
case class CentralWavelengthConfig(
  centralWavelength: Wavelength,
  exposureTimeMode:  ExposureTimeMode,
  coadds:            PosInt
) derives Eq

object CentralWavelengthConfig:

  given HashBytes[CentralWavelengthConfig] with
    def hashBytes(a: CentralWavelengthConfig): Array[Byte] =
      Array.concat(
        a.centralWavelength.hashBytes,
        a.exposureTimeMode.hashBytes,
        a.coadds.value.hashBytes
      )

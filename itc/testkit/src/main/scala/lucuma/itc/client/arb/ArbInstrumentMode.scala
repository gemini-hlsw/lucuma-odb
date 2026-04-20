// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client
package arb

import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.arb.ArbExposureTimeMode
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.arb.ArbGmosCcdMode
import lucuma.core.util.arb.ArbEnumerated
import lucuma.itc.ItcGhostDetector
import lucuma.itc.arb.ArbItcGhostDetector
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbInstrumentMode {
  import ArbEnumerated.given
  import ArbExposureTimeMode.given
  import ArbGmosFpu.given
  import ArbGmosCcdMode.given
  import ArbWavelength.given
  import ArbItcGhostDetector.given

  import InstrumentMode.GmosNorthSpectroscopy
  import InstrumentMode.GmosSouthSpectroscopy
  import InstrumentMode.GmosNorthImaging
  import InstrumentMode.GmosSouthImaging
  import InstrumentMode.Flamingos2Spectroscopy
  import InstrumentMode.Flamingos2Imaging
  import InstrumentMode.Igrins2Spectroscopy
  import InstrumentMode.GhostSpectroscopy

  given Arbitrary[GmosNorthSpectroscopy] =
    Arbitrary {
      for {
        et <- arbitrary[ExposureTimeMode]
        cw <- arbitrary[Wavelength]
        g  <- arbitrary[GmosNorthGrating]
        f  <- arbitrary[Option[GmosNorthFilter]]
        u  <- arbitrary[GmosFpu.North]
        c  <- arbitrary[Option[GmosCcdMode]]
        r  <- arbitrary[Option[GmosRoi]]
        p  <- arbitrary[PortDisposition]
      } yield GmosNorthSpectroscopy(et, cw, g, f, u, c, r, p)
    }

  given Cogen[GmosNorthSpectroscopy] =
    Cogen[
      (
        ExposureTimeMode,
        Wavelength,
        GmosNorthGrating,
        Option[GmosNorthFilter],
        GmosFpu.North,
        PortDisposition
      )
    ].contramap { a =>
      (
        a.exposureTimeMode,
        a.centralWavelength,
        a.grating,
        a.filter,
        a.fpu,
        a.port
      )
    }

  given Arbitrary[GmosSouthSpectroscopy] =
    Arbitrary {
      for {
        et <- arbitrary[ExposureTimeMode]
        cw <- arbitrary[Wavelength]
        g  <- arbitrary[GmosSouthGrating]
        f  <- arbitrary[Option[GmosSouthFilter]]
        u  <- arbitrary[GmosFpu.South]
        c  <- arbitrary[Option[GmosCcdMode]]
        r  <- arbitrary[Option[GmosRoi]]
        p  <- arbitrary[PortDisposition]
      } yield GmosSouthSpectroscopy(et, cw, g, f, u, c, r, p)
    }

  given Cogen[GmosSouthSpectroscopy] =
    Cogen[
      (
        ExposureTimeMode,
        Wavelength,
        GmosSouthGrating,
        Option[GmosSouthFilter],
        GmosFpu.South,
        PortDisposition
      )
    ].contramap { a =>
      (
        a.exposureTimeMode,
        a.centralWavelength,
        a.grating,
        a.filter,
        a.fpu,
        a.port
      )
    }

  given Arbitrary[GmosNorthImaging] =
    Arbitrary {
      for {
        et <- arbitrary[ExposureTimeMode]
        f  <- arbitrary[GmosNorthFilter]
        c  <- arbitrary[Option[GmosCcdMode]]
        p  <- arbitrary[PortDisposition]
      } yield GmosNorthImaging(et, f, c, p)
    }

  given Cogen[GmosNorthImaging] =
    Cogen[(ExposureTimeMode, GmosNorthFilter, Option[GmosCcdMode], PortDisposition)]
      .contramap(a => (a.exposureTimeMode, a.filter, a.ccdMode, a.port))

  given Arbitrary[GmosSouthImaging] =
    Arbitrary {
      for {
        et <- arbitrary[ExposureTimeMode]
        f  <- arbitrary[GmosSouthFilter]
        c  <- arbitrary[Option[GmosCcdMode]]
        p  <- arbitrary[PortDisposition]
      } yield GmosSouthImaging(et, f, c, p)
    }

  given Cogen[GmosSouthImaging] =
    Cogen[(ExposureTimeMode, GmosSouthFilter, Option[GmosCcdMode], PortDisposition)]
      .contramap(a => (a.exposureTimeMode, a.filter, a.ccdMode, a.port))

  given Arbitrary[Flamingos2Spectroscopy] =
    Arbitrary {
      for {
        et <- arbitrary[ExposureTimeMode]
        d  <- arbitrary[Flamingos2Disperser]
        f  <- arbitrary[Flamingos2Filter]
        fp <- arbitrary[Flamingos2Fpu]
        p  <- arbitrary[PortDisposition]
      } yield Flamingos2Spectroscopy(et, d, f, fp, p)
    }

  given Cogen[Flamingos2Spectroscopy] =
    Cogen[(ExposureTimeMode, Flamingos2Disperser, Flamingos2Filter, Flamingos2Fpu, PortDisposition)]
      .contramap(a => (a.exposureTimeMode, a.disperser, a.filter, a.fpu, a.port))

  given Arbitrary[Flamingos2Imaging] =
    Arbitrary {
      for {
        et <- arbitrary[ExposureTimeMode]
        f  <- arbitrary[Flamingos2Filter]
        rm <- arbitrary[Flamingos2ReadMode]
        p  <- arbitrary[PortDisposition]
      } yield Flamingos2Imaging(et, f, rm, p)
    }

  given Cogen[Flamingos2Imaging] =
    Cogen[(ExposureTimeMode, Flamingos2Filter, Flamingos2ReadMode, PortDisposition)]
      .contramap(a => (a.exposureTimeMode, a.filter, a.readMode, a.port))

  given Arbitrary[Igrins2Spectroscopy] =
    Arbitrary {
      for {
        et <- arbitrary[ExposureTimeMode]
        p  <- arbitrary[PortDisposition]
      } yield Igrins2Spectroscopy(et, p)
    }

  given Cogen[Igrins2Spectroscopy] =
    Cogen[(ExposureTimeMode, PortDisposition)]
      .contramap(a => (a.exposureTimeMode, a.port))

  given Arbitrary[GhostSpectroscopy] =
    Arbitrary {
      for {
        re <- arbitrary[GhostResolutionMode]
        r  <- arbitrary[ItcGhostDetector]
        b  <- arbitrary[ItcGhostDetector]
      } yield GhostSpectroscopy(re, r, b)
    }

  given Cogen[GhostSpectroscopy] =
    Cogen[
      (
        GhostResolutionMode,
        ItcGhostDetector,
        ItcGhostDetector
      )
    ].contramap(a => (a.resolutionMode, a.redDetector, a.blueDetector))

  given Arbitrary[InstrumentMode] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[GmosNorthSpectroscopy],
        arbitrary[GmosSouthSpectroscopy],
        arbitrary[GmosNorthImaging],
        arbitrary[GmosSouthImaging],
        arbitrary[Flamingos2Spectroscopy],
        arbitrary[Flamingos2Imaging],
        arbitrary[Igrins2Spectroscopy],
        arbitrary[GhostSpectroscopy]
      )
    }

  given Cogen[InstrumentMode] =
    Cogen[
      (
        Option[GmosNorthSpectroscopy],
        Option[GmosSouthSpectroscopy],
        Option[GmosNorthImaging],
        Option[GmosSouthImaging],
        Option[Flamingos2Spectroscopy],
        Option[Flamingos2Imaging],
        Option[Igrins2Spectroscopy],
        Option[GhostSpectroscopy]
      )
    ].contramap { a =>
      (
        InstrumentMode.gmosNorthSpectroscopy.getOption(a),
        InstrumentMode.gmosSouthSpectroscopy.getOption(a),
        InstrumentMode.gmosNorthImaging.getOption(a),
        InstrumentMode.gmosSouthImaging.getOption(a),
        InstrumentMode.flamingos2Spectroscopy.getOption(a),
        InstrumentMode.flamingos2Imaging.getOption(a),
        InstrumentMode.igrins2Spectroscopy.getOption(a),
        InstrumentMode.ghostSpectroscopy.getOption(a)
      )
    }
}

object ArbInstrumentMode extends ArbInstrumentMode

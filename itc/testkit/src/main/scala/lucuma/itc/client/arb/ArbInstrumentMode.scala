// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client
package arb

import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.arb.ArbExposureTimeMode
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.arb.ArbGmosCcdMode
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbInstrumentMode {
  import ArbEnumerated.given
  import ArbExposureTimeMode.given
  import ArbGmosFpu.given
  import ArbGmosCcdMode.given
  import ArbWavelength.given

  import InstrumentMode.GmosNorthSpectroscopy
  import InstrumentMode.GmosSouthSpectroscopy
  import InstrumentMode.GmosNorthImaging
  import InstrumentMode.GmosSouthImaging
  import InstrumentMode.Igrins2Spectroscopy

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
      } yield GmosNorthSpectroscopy(et, cw, g, f, u, c, r)
    }

  given Cogen[GmosNorthSpectroscopy] =
    Cogen[
      (
        ExposureTimeMode,
        Wavelength,
        GmosNorthGrating,
        Option[GmosNorthFilter],
        GmosFpu.North
      )
    ].contramap { a =>
      (
        a.exposureTimeMode,
        a.centralWavelength,
        a.grating,
        a.filter,
        a.fpu
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
      } yield GmosSouthSpectroscopy(et, cw, g, f, u, c, r)
    }

  given Cogen[GmosSouthSpectroscopy] =
    Cogen[
      (
        ExposureTimeMode,
        Wavelength,
        GmosSouthGrating,
        Option[GmosSouthFilter],
        GmosFpu.South
      )
    ].contramap { a =>
      (
        a.exposureTimeMode,
        a.centralWavelength,
        a.grating,
        a.filter,
        a.fpu
      )
    }

  given Arbitrary[GmosNorthImaging] =
    Arbitrary {
      for {
        et <- arbitrary[ExposureTimeMode]
        f  <- arbitrary[GmosNorthFilter]
        c  <- arbitrary[Option[GmosCcdMode]]
      } yield GmosNorthImaging(et, f, c)
    }

  given Cogen[GmosNorthImaging] =
    Cogen[(ExposureTimeMode, GmosNorthFilter, Option[GmosCcdMode])].contramap(a =>
      (a.exposureTimeMode, a.filter, a.ccdMode)
    )

  given Arbitrary[GmosSouthImaging] =
    Arbitrary {
      for {
        et <- arbitrary[ExposureTimeMode]
        f  <- arbitrary[GmosSouthFilter]
        c  <- arbitrary[Option[GmosCcdMode]]
      } yield GmosSouthImaging(et, f, c)
    }

  given Cogen[GmosSouthImaging] =
    Cogen[(ExposureTimeMode, GmosSouthFilter, Option[GmosCcdMode])].contramap(a =>
      (a.exposureTimeMode, a.filter, a.ccdMode)
    )

  given Arbitrary[Igrins2Spectroscopy] =
    Arbitrary {
      for {
        et <- arbitrary[ExposureTimeMode]
      } yield Igrins2Spectroscopy(et)
    }

  given Cogen[Igrins2Spectroscopy] =
    Cogen[ExposureTimeMode].contramap(_.exposureTimeMode)

  given Arbitrary[InstrumentMode] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[GmosNorthSpectroscopy],
        arbitrary[GmosSouthSpectroscopy],
        arbitrary[GmosNorthImaging],
        arbitrary[GmosSouthImaging],
        arbitrary[Igrins2Spectroscopy]
      )
    }

  given Cogen[InstrumentMode] =
    Cogen[
      (
        Option[GmosNorthSpectroscopy],
        Option[GmosSouthSpectroscopy],
        Option[GmosNorthImaging],
        Option[GmosSouthImaging],
        Option[Igrins2Spectroscopy]
      )
    ].contramap { a =>
      (
        InstrumentMode.gmosNorthSpectroscopy.getOption(a),
        InstrumentMode.gmosSouthSpectroscopy.getOption(a),
        InstrumentMode.gmosNorthImaging.getOption(a),
        InstrumentMode.gmosSouthImaging.getOption(a),
        InstrumentMode.igrins2Spectroscopy.getOption(a)
      )
    }
}

object ArbInstrumentMode extends ArbInstrumentMode

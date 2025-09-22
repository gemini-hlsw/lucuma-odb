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
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.arb.ArbGmosCcdMode
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbInstrumentMode {
  import ArbEnumerated.given
  import ArbGmosFpu.given
  import ArbGmosCcdMode.given
  import ArbWavelength.given

  import InstrumentMode.GmosNorthSpectroscopy
  import InstrumentMode.GmosSouthSpectroscopy
  import InstrumentMode.GmosNorthImaging
  import InstrumentMode.GmosSouthImaging

  given Arbitrary[GmosNorthSpectroscopy] =
    Arbitrary {
      for {
        cw <- arbitrary[Wavelength]
        g  <- arbitrary[GmosNorthGrating]
        f  <- arbitrary[Option[GmosNorthFilter]]
        u  <- arbitrary[GmosFpu.North]
        c  <- arbitrary[Option[GmosCcdMode]]
        r  <- arbitrary[Option[GmosRoi]]
      } yield GmosNorthSpectroscopy(cw, g, f, u, c, r)
    }

  given Cogen[GmosNorthSpectroscopy] =
    Cogen[
      (
        Wavelength,
        GmosNorthGrating,
        Option[GmosNorthFilter],
        GmosFpu.North
      )
    ].contramap { a =>
      (
        a.centralWavelength,
        a.grating,
        a.filter,
        a.fpu
      )
    }

  given Arbitrary[GmosSouthSpectroscopy] =
    Arbitrary {
      for {
        cw <- arbitrary[Wavelength]
        g  <- arbitrary[GmosSouthGrating]
        f  <- arbitrary[Option[GmosSouthFilter]]
        u  <- arbitrary[GmosFpu.South]
        c  <- arbitrary[Option[GmosCcdMode]]
        r  <- arbitrary[Option[GmosRoi]]
      } yield GmosSouthSpectroscopy(cw, g, f, u, c, r)
    }

  given Cogen[GmosSouthSpectroscopy] =
    Cogen[
      (
        Wavelength,
        GmosSouthGrating,
        Option[GmosSouthFilter],
        GmosFpu.South
      )
    ].contramap { a =>
      (
        a.centralWavelength,
        a.grating,
        a.filter,
        a.fpu
      )
    }

  given Arbitrary[GmosNorthImaging] =
    Arbitrary {
      for {
        f <- arbitrary[GmosNorthFilter]
        c <- arbitrary[Option[GmosCcdMode]]
      } yield GmosNorthImaging(f, c)
    }

  given Cogen[GmosNorthImaging] =
    Cogen[(GmosNorthFilter, Option[GmosCcdMode])].contramap(a => (a.filter, a.ccdMode))

  given Arbitrary[GmosSouthImaging] =
    Arbitrary {
      for {
        f <- arbitrary[GmosSouthFilter]
        c <- arbitrary[Option[GmosCcdMode]]
      } yield GmosSouthImaging(f, c)
    }

  given Cogen[GmosSouthImaging] =
    Cogen[(GmosSouthFilter, Option[GmosCcdMode])].contramap(a => (a.filter, a.ccdMode))

  given Arbitrary[InstrumentMode] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[GmosNorthSpectroscopy],
        arbitrary[GmosSouthSpectroscopy],
        arbitrary[GmosNorthImaging],
        arbitrary[GmosSouthImaging]
      )
    }

  given Cogen[InstrumentMode] =
    Cogen[
      (
        Option[GmosNorthSpectroscopy],
        Option[GmosSouthSpectroscopy],
        Option[GmosNorthImaging],
        Option[GmosSouthImaging]
      )
    ].contramap { a =>
      (
        InstrumentMode.gmosNorthSpectroscopy.getOption(a),
        InstrumentMode.gmosSouthSpectroscopy.getOption(a),
        InstrumentMode.gmosNorthImaging.getOption(a),
        InstrumentMode.gmosSouthImaging.getOption(a)
      )
    }
}

object ArbInstrumentMode extends ArbInstrumentMode

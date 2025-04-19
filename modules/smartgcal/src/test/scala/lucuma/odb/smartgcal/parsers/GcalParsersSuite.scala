// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.data.NonEmptySet
import cats.syntax.either.*
import cats.syntax.show.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.TimeSpan
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig

final class GcalParsersSuite extends munit.FunSuite {

  import file.*

  test("simple success, continuum") {
    val definition = "2,None,Visible,Quartz Halogen,Closed,4,1,Night"

    assertEquals(
      legacyValue.parseAll(definition),
      Right(SmartGcalValue(
        Gcal(
          Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W),
          GcalFilter.None,
          GcalDiffuser.Visible,
          GcalShutter.Closed
        ),
        GcalBaselineType.Night,
        PosInt.unsafeFrom(2),
        LegacyInstrumentConfig(
          TimeSpan.unsafeFromMicroseconds(4_000_000L),
          PosInt.unsafeFrom(1)
        )
      )
    ))
  }

  test("simple success, arcs") {
    val definition = "2,ND4-5,Visible,Ar arc;Xe arc,Open,4.3,1,Day"

    assertEquals(
      legacyValue.parseAll(definition),
      Right(SmartGcalValue(
        Gcal(
          Gcal.Lamp.fromArcs(NonEmptySet.of(GcalArc.ArArc, GcalArc.XeArc)),
          GcalFilter.Nd45,
          GcalDiffuser.Visible,
          GcalShutter.Open
        ),
        GcalBaselineType.Day,
        PosInt.unsafeFrom(2),
        LegacyInstrumentConfig(
          TimeSpan.unsafeFromMicroseconds(4_300_000L),
          PosInt.unsafeFrom(1)
        )
      )
    ))
  }

  test("simple failure") {
    val definition = "2,ND4.5,Visible,Ar arc;Xe arc,Open,4.3,1,Day"

    assertEquals(
      legacyValue.parseAll(definition).leftMap(_.show),
      Left(
        """2,ND4.5,Visible,Ar arc;Xe arc,Open,4.3,1,Day
          |  ^
          |expectation:
          |* context: Gcal filter, must match one of the strings: {"GMOS balance", "HROS balance", "ND1.0", "ND1.6", "ND2.0", "ND3.0", "ND4-5", "ND4.0", "ND5.0", "NIR balance", "None", "none"}""".stripMargin
      )
    )
  }

  test("simple failure 2") {
    val definition = "2,ND4-5,Visible,Ar arc;Xe arc,Open,4.3,1,"

    assertEquals(
      legacyValue.parseAll(definition).leftMap(_.show),
      Left(
        """2,ND4-5,Visible,Ar arc;Xe arc,Open,4.3,1,
          |                                         ^
          |expectation:
          |* context: Gcal baseline type, must match one of the strings: {"Day", "Night"}""".stripMargin
      )
    )
  }
}

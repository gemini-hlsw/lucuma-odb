// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.itc.AltairParameters
import lucuma.itc.service.ItcObservingConditions
import munit.FunSuite

class AltairCodecSuite extends FunSuite:

  private val separation: Angle           = Angle.fromDoubleArcseconds(3.5)
  private val brightness: BrightnessValue = BrightnessValue.unsafeFrom(12.5)

  private val conditions: ItcObservingConditions =
    ItcObservingConditions(
      BigDecimal("0.6"),
      BigDecimal("0.3"),
      WaterVapor.Median,
      SkyBackground.Dark,
      1.5
    )

  test("NGS encodes the legacy AltairParameters block with the chosen field lens"):
    assertEquals(
      codecs.encodeAltair(AltairParameters.Ngs(separation, brightness, FieldLens.Out).some),
      json"""{ "guideStarSeparation": 3.5, "guideStarMagnitude": 12.5, "fieldLens": "OUT", "wfsMode": "NGS" }"""
    )

  test("LGS always encodes the field lens in"):
    assertEquals(
      codecs.encodeAltair(AltairParameters.Lgs(separation, brightness).some),
      json"""{ "guideStarSeparation": 3.5, "guideStarMagnitude": 12.5, "fieldLens": "IN", "wfsMode": "LGS" }"""
    )

  test("LGS+P1 and no Altair both leave the legacy ITC without Altair"):
    assertEquals(codecs.encodeAltair(AltairParameters.LgsP1.some), Json.Null)
    assertEquals(codecs.encodeAltair(None), Json.Null)

  test("conditions go out with the exact image quality"):
    val json: Json = codecs.encodeConditions(conditions, None)
    assertEquals(json.asObject.get.keys.toSet, Set("exactiq", "exactcc", "wv", "sb", "airmass"))
    assertEquals(json.hcursor.downField("exactiq").downField("arcsec").as[BigDecimal],
                 Right(BigDecimal("0.6"))
    )
    assertEquals(
      codecs.encodeConditions(conditions, AltairParameters.Lgs(separation, brightness).some),
      json
    )

  test("LGS+P1 goes out at the legacy 20% image quality bin instead"):
    val json: Json = codecs.encodeConditions(conditions, AltairParameters.LgsP1.some)
    assertEquals(json.hcursor.downField("iq").as[String], Right("PERCENT_20"))
    assert(json.hcursor.downField("exactiq").failed)

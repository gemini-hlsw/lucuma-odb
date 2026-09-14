// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.itc.AltairParameters
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcImageQuality
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import munit.FunSuite

class AltairCodecSuite extends FunSuite:

  private val separation: Angle           = Angle.fromDoubleArcseconds(3.5)
  private val brightness: BrightnessValue = BrightnessValue.unsafeFrom(12.5)

  private val conditions: ItcObservingConditions =
    ItcObservingConditions(
      ItcImageQuality.Exact(BigDecimal("0.6")),
      BigDecimal("0.3"),
      WaterVapor.Median,
      SkyBackground.Dark,
      1.5
    )

  private val gnirsImaging: ObservingMode.ImagingMode.Gnirs =
    ObservingMode.ImagingMode.Gnirs(
      GnirsFilter.K,
      GnirsCamera.LongBlue,
      GnirsReadMode.Bright,
      GnirsWellDepth.Shallow,
      PosInt.unsafeFrom(1),
      PortDisposition.Bottom,
      None
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

  test("exact image quality goes out as exactiq"):
    val fields: Set[String] = conditions.asJson.asObject.get.keys.toSet
    assertEquals(fields, Set("exactiq", "exactcc", "wv", "sb", "airmass"))
    assertEquals(conditions.asJson.hcursor.downField("exactiq").downField("arcsec").as[BigDecimal], Right(BigDecimal("0.6")))

  test("the 20% bin goes out as the legacy iq percentile"):
    val json: Json = conditions.copy(iq = ItcImageQuality.Percentile20).asJson
    assertEquals(json.hcursor.downField("iq").as[String], Right("PERCENT_20"))
    assert(json.hcursor.downField("exactiq").failed)

  test("LGS+P1 is computed without Altair at the 20% bin, other modes keep their conditions"):
    val p1: ObservingMode = gnirsImaging.copy(altair = AltairParameters.LgsP1.some)
    assertEquals(conditionsFor(p1, conditions).iq, ItcImageQuality.Percentile20)
    val lgs: ObservingMode = gnirsImaging.copy(altair = AltairParameters.Lgs(separation, brightness).some)
    assertEquals(conditionsFor(lgs, conditions), conditions)
    assertEquals(conditionsFor(gnirsImaging, conditions), conditions)

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.AltairParameters
import lucuma.itc.client.json.encoders.given
import munit.FunSuite

class AltairParametersEncoderSuite extends FunSuite:

  private val separation: Angle           = Angle.fromDoubleArcseconds(3.5)
  private val brightness: BrightnessValue = BrightnessValue.unsafeFrom(12.5)

  private val imaging: InstrumentMode.GnirsImaging =
    InstrumentMode.GnirsImaging(
      ExposureTimeMode.SignalToNoiseMode(
        SignalToNoise.unsafeFromBigDecimalExact(100),
        Wavelength.fromIntNanometers(2200).get
      ),
      GnirsFilter.K,
      GnirsCamera.LongBlue,
      GnirsReadMode.Bright,
      GnirsWellDepth.Shallow,
      PosInt.unsafeFrom(1),
      PortDisposition.Bottom,
      None
    )

  test("NGS carries separation, brightness and field lens"):
    assertEquals(
      (AltairParameters.Ngs(separation, brightness, FieldLens.Out): AltairParameters).asJson,
      json"""{ "mode": "NGS", "guideStarSeparation": { "arcseconds": 3.5 }, "guideStarBrightness": 12.5, "fieldLens": "OUT" }"""
    )

  test("LGS carries separation and brightness only"):
    assertEquals(
      (AltairParameters.Lgs(separation, brightness): AltairParameters).asJson,
      json"""{ "mode": "LGS", "guideStarSeparation": { "arcseconds": 3.5 }, "guideStarBrightness": 12.5 }"""
    )

  test("LGS+P1 is just the mode"):
    assertEquals((AltairParameters.LgsP1: AltairParameters).asJson, json"""{ "mode": "LGS_P1" }""")

  test("GNIRS modes only mention altair when it is set"):
    assert(imaging.asJson.hcursor.downField("altair").failed)
    assertEquals(
      imaging.copy(altair = AltairParameters.LgsP1.some).asJson.hcursor.downField("altair").focus,
      json"""{ "mode": "LGS_P1" }""".some
    )

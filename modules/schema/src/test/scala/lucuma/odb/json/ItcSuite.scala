// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.parser.decode
import io.circe.syntax.*
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.odb.data.ItcAcquisition
import lucuma.odb.data.ItcResult
import lucuma.odb.data.ItcScience
import lucuma.odb.data.arb.ArbItc
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

class ItcSuite extends DisciplineSuite with ArbitraryInstances:
  import ArbItc.given
  import itc.given
  import time.query.given
  import wavelength.query.given

  // The composite `Itc` has no JSON codec (it is assembled from separate
  // columns); its two parts are stored and cached independently, so we exercise
  // their codecs directly.
  checkAll("ItcResultCodec", CodecTests[ItcResult].codec)
  checkAll("ItcScienceCodec", CodecTests[ItcScience].codec)
  checkAll("ItcAcquisitionAvailableCodec", CodecTests[ItcAcquisition.Available].codec)

  property("peakPixel survives a round trip"):
    forAll: (r: ItcResult) =>
      assertEquals(
        decode[ItcResult](r.asJson.spaces2).map(_.peakPixel),
        Right(r.peakPixel)
      )

  // Rows cached before the field existed must still decode.
  test("peakPixel is absent from legacy JSON"):
    val legacy =
      """
        {
          "targetId": "t-100",
          "exposureTime": { "microseconds": 1000000 },
          "exposureCount": 3,
          "signalToNoiseAt": null
        }
      """
    assertEquals(decode[ItcResult](legacy).map(_.peakPixel), Right(None))

  test("peakPixel is absent from flat peakPixelFlux JSON"):
    val flat =
      """
        {
          "targetId": "t-100",
          "exposureTime": { "microseconds": 1000000 },
          "exposureCount": 3,
          "signalToNoiseAt": null,
          "peakPixelFlux": 2500.0
        }
      """
    assertEquals(decode[ItcResult](flat).map(_.peakPixel), Right(None))

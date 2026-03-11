// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.kernel.laws.discipline.EqTests
import io.circe.Json
import io.circe.literal.*
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.itc.arb.ArbError.given
import lucuma.itc.client.json.decoders.given
import lucuma.odb.json.wavelength.query.given

class ErrorsSuite extends munit.DisciplineSuite with ArbitraryInstances:
  checkAll("Error", EqTests[Error].eqv)
  checkAll("Codec[Error]", CodecTests[Error].codec)

  test("Decoder[TargetIntegrationTimeOutcome] decodes WavelengthAtOutOfRange"):
    val payload: Json = json"""
      {
        "errorCode": "WAVELENGTH_AT_OUT_OF_RANGE",
        "message": "The requested wavelength falls outside the instrument's wavelength coverage.",
        "wellHalfFilledSeconds": null,
        "wavelength": { "picometers": 1800000 }
      }
    """

    val result = payload.as[TargetIntegrationTimeOutcome]
    assert(result.isRight)

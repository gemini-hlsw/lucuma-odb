// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength.*
import munit.DisciplineSuite

abstract class WavelengthSuite(using Encoder[Wavelength]) extends DisciplineSuite with ArbitraryInstances {

  import wavelength.decoder.given

  checkAll("WavelengthCodec", CodecTests[Wavelength].codec)

  test("all wavelength encoders produce the same wavelength") {
    conversionTest[Wavelength]()
  }

}

class WavelengthQuerySuite extends WavelengthSuite(using
  wavelength.query.Encoder_Wavelength
)

class WavelengthTransportSuite extends WavelengthSuite(using
  wavelength.transport.Encoder_Wavelength
)

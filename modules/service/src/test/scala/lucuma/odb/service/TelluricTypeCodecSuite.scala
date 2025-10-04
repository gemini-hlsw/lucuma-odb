// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.TelluricType
import lucuma.odb.graphql.input.arb.ArbFlamingos2LongSlitInput.given
import munit.DisciplineSuite

class TelluricTypeCodecSuite extends DisciplineSuite with ArbitraryInstances {

  // Import the Circe encoders/decoders from Flamingos2LongSlitService
  import Flamingos2LongSlitService.given

  // Use unserializableCodec because semiauto-derived codecs aren't serializable
  checkAll("Codec", CodecTests[TelluricType].unserializableCodec)

}

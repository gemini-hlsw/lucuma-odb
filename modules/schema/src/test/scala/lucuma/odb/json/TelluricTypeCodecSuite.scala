// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.TelluricType
import lucuma.core.model.arb.ArbTelluricType.given
import munit.DisciplineSuite

class TelluricTypeQueryCodecSuite extends DisciplineSuite with ArbitraryInstances:
  import tellurictype.query.given

  checkAll("QueryCodec", CodecTests[TelluricType].unserializableCodec)

class TelluricTypeTransportCodecSuite extends DisciplineSuite with ArbitraryInstances:
  import tellurictype.transport.given

  checkAll("TransportCodec", CodecTests[TelluricType].unserializableCodec)

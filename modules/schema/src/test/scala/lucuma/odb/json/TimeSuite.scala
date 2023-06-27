// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan
import munit.DisciplineSuite

abstract class TimeSuite(using Encoder[TimeSpan]) extends DisciplineSuite with ArbitraryInstances {

  import ArbTimeSpan.given
  import time.decoder.given

  checkAll("TimeSpanCodec", CodecTests[TimeSpan].codec)
}

class TimeQuerySuite extends TimeSuite(using time.query.Encoder_TimeSpan)

class TimeTransportSuite extends TimeSuite(using time.transport.Encoder_TimeSpan)

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import munit.DisciplineSuite

abstract class OffsetSuite[A](using Encoder[Offset.Component[A]], Encoder[Offset]) extends DisciplineSuite with ArbitraryInstances {

  import ArbOffset.given
  import offset.decoder.given

  checkAll("OffsetCodec",        CodecTests[Offset].codec)

  test("all offset encoders produce the same offset") {
    conversionTest[Offset.Component[A]]()
  }

}

class OffsetQuerySuite extends OffsetSuite[Offset.P](using
  offset.query.Encoder_Offset_Component,
  offset.query.Encoder_Offset
)

class OffsetTransportSuite extends OffsetSuite[Offset.P](using
  offset.transport.Encoder_Offset_Component,
  offset.transport.Encoder_Offset
)

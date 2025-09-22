// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Error
import io.circe.parser.decode
import io.circe.syntax.*

class ItcGraphSuite extends munit.FunSuite {
  import lucuma.itc.legacy.given
  import lucuma.itc.legacy.*

  val source      = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/graphs.json"))
  val expected    = source.mkString
  val graphPprint = pprint.copy(
    additionalHandlers = { case value: ItcSeries =>
      pprint.Tree.Literal(ItcSeries(value.title, value.seriesType, value.data.take(2)).toString)
    }
  )
  test("decode response") {
    assertEquals(
      2.asRight,
      decode[GraphsRemoteResult](expected).map(_.groups.flatMap(_.graphs).length)
    )
  }

}

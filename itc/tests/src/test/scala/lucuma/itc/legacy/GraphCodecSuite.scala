// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import io.circe.parser.decode
import lucuma.itc.ItcGraph
import lucuma.itc.legacy.codecs.given
import munit.FunSuite

class GraphCodecSuite extends FunSuite:

  private def chart(start: Double, end: Double, dataY: List[Double]): String =
    s"""{
       |  "chartType": { "S2NChart": {} },
       |  "series": [
       |    {
       |      "title": "Final S/N BB(B)",
       |      "dataType": { "FinalS2NData": {} },
       |      "dataY": ${dataY.mkString("[", ",", "]")},
       |      "xAxis": { "start": $start, "end": $end, "count": ${dataY.length} }
       |    }
       |  ]
       |}""".stripMargin

  test("a series reaching below 0 nm decodes without its non-positive samples"):
    val json = chart(-2.0, 8.0, List(0.0, 0.0, 0.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0))
    val s    = decode[ItcGraph](json).fold(e => fail(s"did not decode: $e"), _.series.head)
    assertEquals(s.xAxis.start, 1.0)
    assertEquals(s.xAxis.count, 8)
    assertEquals(s.dataY.length, 8)
    assertEquals(s.dataY.head, 3.0)

  test("a series entirely below 0 nm fails to decode"):
    val json = chart(-5.0, -1.0, List(1.0, 2.0, 3.0, 4.0, 5.0))
    val msg  = decode[ItcGraph](json).fold(_.getMessage, _ => fail("expected a decoding failure"))
    assert(msg.contains("Final S/N BB(B)"), msg)

  test("a positive series decodes unchanged"):
    val json = chart(1.0, 5.0, List(1.0, 2.0, 3.0, 4.0, 5.0))
    val s    = decode[ItcGraph](json).fold(e => fail(s"did not decode: $e"), _.series.head)
    assertEquals(s.xAxis.start, 1.0)
    assertEquals(s.dataY.length, 5)

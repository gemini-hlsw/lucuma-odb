// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

//
// 500,   0
// 500,   0
// 500,  15
// 500,  15
// 500, -15
// 500, -15
// --
// 505,   0
// 505,   0
// 505,  15
// 505,  15
// 505, -15
// 505, -15
// --
// 495,   0
// 495,   0
// 495,  15
// 495,  15
// 495, -15
// 495, -15
// --
// 500,   0
// 500,  15
//

class executionSciGmosNorth_20x_3d_6q extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      10.minTimeSpan,
      PosInt.unsafeFrom(20)
    )

  extension (Δλnm: Int)
    def description: NonEmptyString =
      NonEmptyString.unsafeFrom(s"${Δλnm}.000 nm")

  test("simple generation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    val AtomQuery: String =
      s"""
        description
        steps {
          instrumentConfig {
            centralWavelength { nanometers }
          }
          telescopeConfig {
            offset {
              q { arcseconds }
            }
          }
        }
      """

    def step(dither: Int, q: Int): Json =
      json"""
        {
          "instrumentConfig": {
            "centralWavelength": {
              "nanometers": ${Json.fromBigDecimal(BigDecimal(500 + dither).setScale(3))}
            }
          },
          "telescopeConfig": {
            "offset": {
              "q": {
                "arcseconds": ${Json.fromBigDecimal(BigDecimal(q).setScale(6))}
              }
            }
          }
        }
      """

    def atom(dither: Int, q0: Int, qs: Int*): Json =
      json"""
        {
          "description": ${s"$dither.000 nm".asJson},
          "steps": ${(q0 :: q0 :: q0 :: qs.toList).map(q => step(dither, q)).asJson}
        }
      """

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    =
          s"""
            query {
              executionConfig(observationId: "$oid") {
                gmosNorth {
                  science {
                    nextAtom {
                      $AtomQuery
                    }
                    possibleFuture {
                      $AtomQuery
                    }
                  }
                }
              }
            }
          """,
        expected =
          json"""
            {
              "executionConfig": {
                "gmosNorth": {
                  "science": {
                    "nextAtom": ${atom(0, 0, 0, 15, 15, -15, -15)},
                    "possibleFuture": [
                      ${atom( 5,  0,  0, 15, 15, -15, -15)},
                      ${atom(-5,  0,  0, 15, 15, -15, -15)},
                      ${atom( 0,  0, 15)}
                    ]
                  }
                }
              }
            }
          """.asRight
      )
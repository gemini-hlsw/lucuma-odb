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
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

// A slightly unusual test with offsets specified as [ 10, -10, -10, 10 ].
// This amounts to the same thing as [ 10, -10 ], a 1:1 distribution of datasets
// to 10 and -10 spread as evenly as possible over the wavelength dithers.
//
// 500,  10
// 500,  10
// 500, -10
// --
// 505, -10
// 505, -10
// 505,  10
// --
// 495,  10
// 495,  10
// 495, -10
// --
// 500, -10


class executionSciGmosNorth_10x_3d_3q extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  extension (Δλnm: Int)
    def description: NonEmptyString =
      NonEmptyString.unsafeFrom(s"${Δλnm}.000 nm")

  private def createLongerGmosNorthLongSlitObservationAs(
    pid:  Program.Id,
    tid:  Target.Id
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      pi,
      pid,
      List(tid),
      """
        gmosNorthLongSlit: {
          grating: R831_G5302,
          filter: R_PRIME,
          fpu: LONG_SLIT_0_50,
          explicitYBin: TWO,
          centralWavelength: {
            nanometers: 500
          }
          explicitSpatialOffsets: [
            {
              arcseconds: 10.0
            },
            {
              arcseconds: -10.0
            },
            {
              arcseconds: -10.0
            },
            {
              arcseconds: 10.0
            }
          ]
        }
      """
    )

  test("simple generation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createLongerGmosNorthLongSlitObservationAs(p, t)
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
                    "nextAtom": ${atom(0, 10, 10, -10)},
                    "possibleFuture": [
                      ${atom( 5, -10, -10,  10)},
                      ${atom(-5,  10,  10, -10)},
                      ${atom( 0, -10)}
                    ]
                  }
                }
              }
            }
          """.asRight
      )
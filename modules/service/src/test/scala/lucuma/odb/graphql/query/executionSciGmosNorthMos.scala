// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.model.Observation
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

/**
 * GMOS North MOS science generation.
 * The sequence is the same as for longslit but with the custom mask for fpu.
 */
class executionSciGmosNorthMos extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  private val CustomMask: GmosFpuMask.Custom =
    GmosFpuMask.Custom(ToBeDefined, GmosCustomSlitWidth.CustomWidth_0_50)

  // Every generated step carries the custom mask instead of the builtin FPU.
  // The arc, flat and science helpers all derive from this one.
  override def gmosNorthScience(ditherNm: Int): GmosNorth =
    super.gmosNorthScience(ditherNm).copy(fpu = CustomMask.some)

  test("simple generation - limited future"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthMosObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 1.some),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "science" -> Json.obj(
                  // MOS has no default offsets, so every step is on axis.
                  "nextAtom" -> gmosNorthExpectedScienceAtom(ditherNm = 0, 0, 0, 0),
                  "possibleFuture" -> List(gmosNorthExpectedScienceAtom(ditherNm = 5, 0, 0, 0)).asJson,
                  "hasMore" -> true.asJson
                )
              )
            )
          ).asRight
      )

  test("there is an acquisition sequence"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthMosObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          query {
            executionConfig(observationId: "$oid") {
              gmosNorth {
                acquisition {
                  nextAtom {
                    description
                    steps {
                      instrumentConfig {
                        exposure { seconds }
                      }
                    }
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
                  "acquisition": {
                    "nextAtom": {
                      "description": "Initial Acquisition",
                      "steps": [
                        {
                          "instrumentConfig": {
                            "exposure": { "seconds": 30.000000 }
                          }
                        }
                      ]
                    }
                  }
                }
              }
            }
          """.asRight
      )

  private def acquisitionConfigQuery(o: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$o") {
          observingMode {
            gmosNorthMos {
              acquisition {
                filter
                defaultFilter
                explicitFilter
                exposureTimeMode {
                  timeAndCount { time { seconds } count }
                }
              }
            }
          }
        }
      }
    """

  test("acquisition defaults to Time & Count, 30 s, count 10, with a wavelength-derived filter"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthMosObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = acquisitionConfigQuery(oid),
        expected =
          json"""
            {
              "observation": {
                "observingMode": {
                  "gmosNorthMos": {
                    "acquisition": {
                      "filter": "G_PRIME",
                      "defaultFilter": "G_PRIME",
                      "explicitFilter": null,
                      "exposureTimeMode": {
                        "timeAndCount": { "time": { "seconds": 30.000000 }, "count": 10 }
                      }
                    }
                  }
                }
              }
            }
          """.asRight
      )

  test("an explicit acquisition filter overrides the default"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), s"""
               gmosNorthMos: {
                 grating: R831_G5302
                 filter: R_PRIME
                 customMask: { slitWidth: CUSTOM_WIDTH_0_50 }
                 centralWavelength: { nanometers: 500 }
                 explicitYBin: TWO
                 acquisition: { explicitFilter: I_PRIME }
               }
             """)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = acquisitionConfigQuery(oid),
        expected =
          json"""
            {
              "observation": {
                "observingMode": {
                  "gmosNorthMos": {
                    "acquisition": {
                      "filter": "I_PRIME",
                      "defaultFilter": "G_PRIME",
                      "explicitFilter": "I_PRIME",
                      "exposureTimeMode": {
                        "timeAndCount": { "time": { "seconds": 30.000000 }, "count": 10 }
                      }
                    }
                  }
                }
              }
            }
          """.asRight
      )

  test("a signal-to-noise MOS acquisition exposure time mode is rejected"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      _ <- expect(
             user  = pi,
             query = createObservationWithModeQuery(p, List(t), s"""
               gmosNorthMos: {
                 grating: R831_G5302
                 filter: R_PRIME
                 customMask: { slitWidth: CUSTOM_WIDTH_0_50 }
                 centralWavelength: { nanometers: 500 }
                 explicitYBin: TWO
                 acquisition: {
                   exposureTimeMode: {
                     signalToNoise: {
                       value: 10
                       at: { nanometers: 500 }
                     }
                   }
                 }
               }
             """),
             expected = List(
               "Argument 'input.SET.observingMode.gmosNorthMos.acquisition' is invalid: A GMOS North MOS acquisition exposure time mode must be Time & Count."
             ).asLeft
           )
    yield ()

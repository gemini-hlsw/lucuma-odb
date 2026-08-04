// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
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

    pprint.pprintln(gmosNorthExpectedScienceAtom(ditherNm = 0, 0, 15, -15).spaces2)
    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 1.some),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> gmosNorthExpectedScienceAtom(ditherNm = 0, 0, 15, -15),
                  "possibleFuture" -> List(gmosNorthExpectedScienceAtom(ditherNm = 5, 0, 15, -15)).asJson,
                  "hasMore" -> true.asJson
                )
              )
            )
          ).asRight
      )

  test("there is no acquisition sequence"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthMosObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "acquisition" -> Json.Null
              )
            )
          ).asRight
      )

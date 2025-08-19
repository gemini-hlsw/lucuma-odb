// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

class ShortCut_6465 extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  test("two target asterism, one deleted"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t0 <- createTargetWithProfileAs(pi, p)
        t1 <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t0, t1))
        _  <- deleteTargetAs(pi, t1)
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
                  "nextAtom" -> gmosNorthExpectedScienceAtom(ditherNm = 0, 0, 15, -15),
                  "possibleFuture" -> List(gmosNorthExpectedScienceAtom(ditherNm = 5, 0, 15, -15)).asJson,
                  "hasMore" -> true.asJson
                )
              )
            )
          ).asRight
      )

  test("two target asterism, none deleted"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t0 <- createTargetWithProfileAs(pi, p)
        t1 <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t0, t1))
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
                  "nextAtom" -> gmosNorthExpectedScienceAtom(ditherNm = 0, 0, 15, -15),
                  "possibleFuture" -> List(gmosNorthExpectedScienceAtom(ditherNm = 5, 0, 15, -15)).asJson,
                  "hasMore" -> true.asJson
                )
              )
            )
          ).asRight
      )

  test("two target asterism, both deleted"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t0 <- createTargetWithProfileAs(pi, p)
        t1 <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t0, t1))
        _  <- deleteTargetAs(pi, t0)
        _  <- deleteTargetAs(pi, t1)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 1.some),
        expected = List(s"Could not generate a sequence for $oid: observation is missing target").asLeft
      )

  test("no targets at all"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List())
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 1.some),
        expected = List(s"Could not generate a sequence for $oid: observation is missing target").asLeft
      )
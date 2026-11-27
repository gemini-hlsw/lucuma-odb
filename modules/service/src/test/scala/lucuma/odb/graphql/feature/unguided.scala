// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.ags.GuideStarName
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import org.http4s.Request
import org.http4s.Response

class unguided extends query.ExecutionTestSupportForGmos with mutation.UpdateObservationsOps:

  // An unguided observation never searches for a guide star.
  override def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] =
    _ => Resource.eval(IO.raiseError(Exception("Test failure, unexpected call to Gaia!!!")))

  private val ObsTime: Timestamp =
    Timestamp.FromString.getOption("2024-08-25T00:00:00Z").get

  private val ObsDuration: TimeSpan =
    TimeSpan.fromSeconds(32).get

  private val OverrideName: String =
    GuideStarName.gaiaSourceId.reverseGet(1L).value.value

  private val IdGraph: String =
    "observations { id }"

  private def ids(oid: Observation.Id): Json =
    json"""{ "updateObservations": { "observations": [ { "id": $oid } ] } }"""

  private def setProbe(oid: Observation.Id, probe: String, user: User = pi): IO[Unit] =
    updateObservation(user, oid, s"targetEnvironment: { explicitGuideProbe: $probe }", IdGraph, ids(oid).asRight)

  private val observation: IO[(Program.Id, Observation.Id)] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    yield (p, o)

  private def guiding(json: Json): List[String] =
    json.findAllByKey("guiding").flatMap(_.asString)

  private def acquisitionGuiding(oid: Observation.Id): IO[List[String]] =
    query(pi, gmosNorthAcquisitionQuery(oid)).map(guiding)

  private def scienceGuiding(oid: Observation.Id): IO[List[String]] =
    query(pi, gmosNorthScienceQuery(oid)).map(guiding)

  private def validationCodes(oid: Observation.Id): IO[List[String]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow { value { validationErrors { code } } }
          }
        }
      """
    ).map(_.findAllByKey("code").flatMap(_.asString))

  private val timedUnguidedObservation: IO[(Program.Id, Observation.Id)] =
    for
      (pid, oid) <- observation
      _          <- setProbe(oid, "UNGUIDED")
      _          <- setObservationTimeAndDuration(pi, oid, ObsTime.some, ObsDuration.some)
    yield (pid, oid)

  private def expectNoGuideTargets(oid: Observation.Id): IO[Unit] =
    expect(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            targetEnvironment { guideEnvironment { guideTargets { name } } }
          }
        }
      """,
      json"""
        {
          "observation": {
            "targetEnvironment": { "guideEnvironment": { "guideTargets": [] } }
          }
        }
      """.asRight
    )

  test("the probes report the designation, and the default is never unguided"):
    observation.flatMap: (_, oid) =>
      updateObservation(
        pi,
        oid,
        "targetEnvironment: { explicitGuideProbe: UNGUIDED }",
        "observations { targetEnvironment { guideProbe defaultGuideProbe explicitGuideProbe } }",
        json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "targetEnvironment": {
                    "guideProbe": "UNGUIDED",
                    "defaultGuideProbe": "GMOS_OIWFS",
                    "explicitGuideProbe": "UNGUIDED"
                  }
                }
              ]
            }
          }
        """.asRight
      )

  test("no step of an unguided observation guides"):
    for
      (_, oid) <- observation
      before   <- (acquisitionGuiding(oid), scienceGuiding(oid)).mapN(_ ++ _)
      _        <- setProbe(oid, "UNGUIDED")
      acq      <- acquisitionGuiding(oid)
      sci      <- scienceGuiding(oid)
    yield
      assert(before.contains("ENABLED"))
      assert(acq.nonEmpty && acq.forall(_ === "DISABLED"), acq.toString)
      assert(sci.nonEmpty && sci.forall(_ === "DISABLED"), sci.toString)

  test("choosing a probe again restores the guiding the steps had"):
    for
      (_, oid) <- observation
      before   <- (acquisitionGuiding(oid), scienceGuiding(oid)).tupled
      _        <- setProbe(oid, "UNGUIDED")
      _        <- setProbe(oid, "null")
      after    <- (acquisitionGuiding(oid), scienceGuiding(oid)).tupled
    yield assertEquals(after, before)

  test("stored steps keep their guiding and are read with it disabled"):
    for
      (_, oid) <- observation
      _        <- recordVisitAs(serviceUser, oid)
      before   <- (acquisitionGuiding(oid), scienceGuiding(oid)).tupled
      _        <- setProbe(oid, "UNGUIDED")
      masked   <- (acquisitionGuiding(oid), scienceGuiding(oid)).mapN(_ ++ _)
      _        <- setProbe(oid, "null")
      after    <- (acquisitionGuiding(oid), scienceGuiding(oid)).tupled
    yield
      assert(masked.nonEmpty && masked.forall(_ === "DISABLED"), masked.toString)
      assertEquals(after, before)

  test("an unguided observation carries the unguided warning"):
    for
      (pid, oid) <- observation
      _          <- setProbe(oid, "UNGUIDED")
      _          <- runObscalcUpdate(pid, oid)
      unguided   <- validationCodes(oid)
      _          <- setProbe(oid, "null")
      _          <- runObscalcUpdate(pid, oid)
      guided     <- validationCodes(oid)
    yield
      assert(unguided.contains("UNGUIDED"), unguided.toString)
      assert(!guided.contains("UNGUIDED"), guided.toString)

  test("an unguided observation has no guide targets, even with no observation time"):
    for
      (_, oid) <- observation
      _        <- setProbe(oid, "UNGUIDED")
      _        <- expectNoGuideTargets(oid)
    yield ()

  test("an unguided observation has no guide availability"):
    for
      (_, oid) <- observation
      _        <- setProbe(oid, "UNGUIDED")
      _        <- expect(
                    pi,
                    s"""
                      query {
                        observation(observationId: ${oid.asJson}) {
                          targetEnvironment {
                            guideAvailability(start: "2023-01-01T00:00:00Z", end: "2023-01-02T00:00:00Z") { start }
                          }
                        }
                      }
                    """,
                    json"""
                      {
                        "observation": {
                          "targetEnvironment": { "guideAvailability": [] }
                        }
                      }
                    """.asRight
                  )
    yield ()

  test("the background calculation settles an unguided observation that has a time"):
    for
      (pid, oid) <- timedUnguidedObservation
      _          <- runObscalcUpdate(pid, oid)
      codes      <- validationCodes(oid)
      sci        <- scienceGuiding(oid)
    yield
      assertEquals(codes, List("UNGUIDED"))
      assert(sci.nonEmpty && sci.forall(_ === "DISABLED"), sci.toString)

  test("an unguided observation that has a time has no guide targets"):
    timedUnguidedObservation.flatMap: (_, oid) =>
      expectNoGuideTargets(oid)

  test("a guide star override is kept and ignored while unguided"):
    for
      (pid, oid) <- timedUnguidedObservation
      _          <- setCatalogGuideTarget(pi, oid, OverrideName.some)
      _          <- runObscalcUpdate(pid, oid)
      codes      <- validationCodes(oid)
      _          <- expectNoGuideTargets(oid)
      _          <- expect(
                      pi,
                      s"""
                        query {
                          observation(observationId: ${oid.asJson}) {
                            targetEnvironment { guideTargetName }
                          }
                        }
                      """,
                      json"""
                        {
                          "observation": {
                            "targetEnvironment": { "guideTargetName": $OverrideName }
                          }
                        }
                      """.asRight
                    )
    yield assertEquals(codes, List("UNGUIDED"))

  private def posAngleQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: ${oid.asJson}) {
          targetEnvironment { guideEnvironment { posAngle { degrees } } }
        }
      }
    """

  private def averageParallacticUnguided(time: Option[Timestamp]): IO[Observation.Id] =
    for
      (_, oid) <- observation
      _        <- updateObservation(
                    pi,
                    oid,
                    "posAngleConstraint: { mode: AVERAGE_PARALLACTIC }, targetEnvironment: { explicitGuideProbe: UNGUIDED }",
                    IdGraph,
                    ids(oid).asRight
                  )
      _        <- setObservationTimeAndDuration(pi, oid, time, TimeSpan.fromHours(1).get.some)
    yield oid

  // The target is two hours past the meridian from Gemini North at this time.
  private val PastMeridian: Timestamp =
    Timestamp.FromString.getOption("2024-08-24T20:00:00Z").get

  test("an unguided average parallactic observation reports the parallactic angle"):
    for
      oid   <- averageParallacticUnguided(PastMeridian.some)
      angle <- query(pi, posAngleQuery(oid)).map(_.findAllByKey("degrees").flatMap(_.as[BigDecimal].toOption))
    yield
      assertEquals(angle.size, 1)
      assert(angle.forall(a => a > 1 && a < 359), angle.toString)

  test("an unguided average parallactic observation needs an observation time"):
    averageParallacticUnguided(none).flatMap: oid =>
      expect(pi, posAngleQuery(oid), List(s"Observation time not set for observation $oid.").asLeft)

  private def recordedGuiding(oid: Observation.Id): IO[List[String]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            execution {
              atomRecords { matches { steps { matches { telescopeConfig { guiding } } } } }
            }
          }
        }
      """
    ).map(guiding)

  private def executeFirstScienceAtom(oid: Observation.Id): IO[Unit] =
    for
      vid  <- recordVisitAs(serviceUser, oid)
      sids <- firstScienceAtomStepIds(serviceUser, oid)
      _    <- sids.traverse_(addEndStepEvent(_, vid))
    yield ()

  test("steps executed while unguided are recorded as not guided, and stay so"):
    for
      (_, oid) <- observation
      _        <- setProbe(oid, "UNGUIDED")
      _        <- executeFirstScienceAtom(oid)
      recorded <- recordedGuiding(oid)
      _        <- setProbe(oid, "null", staff)
      after    <- recordedGuiding(oid)
      rest     <- scienceGuiding(oid)
    yield
      assert(recorded.nonEmpty && recorded.forall(_ === "DISABLED"), recorded.toString)
      assertEquals(after, recorded)
      assert(rest.contains("ENABLED"), rest.toString)

  test("steps executed while guided keep their guiding when the observation becomes unguided"):
    for
      (_, oid) <- observation
      _        <- executeFirstScienceAtom(oid)
      recorded <- recordedGuiding(oid)
      _        <- setProbe(oid, "UNGUIDED", staff)
      after    <- recordedGuiding(oid)
    yield
      assert(recorded.contains("ENABLED"), recorded.toString)
      assertEquals(after, recorded)

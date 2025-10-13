// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.ags.GuideStarName
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import org.http4s.Request
import org.http4s.Response

trait GuideTargetNameSuite extends ExecutionTestSupport {

  val targetName1: String = GuideStarName.gaiaSourceId.reverseGet(1L).value.value

  val ObsTime: Timestamp = Timestamp.FromString.getOption("2024-08-25T00:00:00Z").get
  val Later: Timestamp = ObsTime.plusSecondsOption(120L).get

  val ObsDuration: TimeSpan = TimeSpan.fromSeconds(32).get

  def guideTargetNameQuery(oid: Observation.Id) =
    s"""
      query {
        observation(observationId: "$oid") {
          targetEnvironment {
            guideTargetName
          }
        }
      }
    """

  def guideTargetNameResult(expectedName: Option[String]): Either[Nothing, Json] =
    json"""
    {
      "observation": {
        "targetEnvironment": {
          "guideTargetName": ${expectedName.asJson}
        }
      }
    }
    """.asRight

  // This just ensures that Gaia is never called
  override def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] =
    _ => Resource.eval(IO.raiseError(Exception("Test failure, unexpected call to Gaia!!!")))

  // Abstract method to be implemented by concrete test suites
  def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id]

  test("no configuration returns null") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        o <- createObservationAs(pi, p)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideTargetNameQuery(oid),
        expected = guideTargetNameResult(none))
    }
  }

  test("no observation time returns null") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, none, ObsDuration.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideTargetNameQuery(oid),
        expected = guideTargetNameResult(none))
    }
  }

  test("no science targets returns null") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        o <- createObservationAs(pi, p, List.empty)
        _ <- setObservationTimeAndDuration(pi, o, ObsTime.some, none)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideTargetNameQuery(oid),
        expected = guideTargetNameResult(none))
    }
  }

  test("no guide target name set returns null") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, ObsTime.some, none)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideTargetNameQuery(oid),
        expected = guideTargetNameResult(none))
    }
  }

  test("set guide target name returns name") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, ObsTime.some, none)
        _ <- setGuideTargetName(pi, o, targetName1.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideTargetNameQuery(oid),
        expected = guideTargetNameResult(targetName1.some))
    }
  }

  test("removing targets invalidates name") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, ObsTime.some, none)
        _ <- setGuideTargetName(pi, o, targetName1.some)
        _ <- updateAsterisms(pi, List(o), add = List.empty, del = List(t), exp = List((o -> List.empty)))
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideTargetNameQuery(oid),
        expected = guideTargetNameResult(none))
    }
  }

  test("unsetting guide target name invalidates name") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, ObsTime.some, none)
        _ <- setGuideTargetName(pi, o, targetName1.some)
        _ <- setGuideTargetName(pi, o, none)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideTargetNameQuery(oid),
        expected = guideTargetNameResult(none))
    }
  }

  test("unsetting observation time invalidates name") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, ObsTime.some, none)
        _ <- setGuideTargetName(pi, o, targetName1.some)
        _ <- setObservationTimeAndDuration(pi, o, none, none)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideTargetNameQuery(oid),
        expected = guideTargetNameResult(none))
    }
  }

  test("changing observation time invalidates name") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, ObsTime.some, none)
        _ <- setGuideTargetName(pi, o, targetName1.some)
        _ <- setObservationTimeAndDuration(pi, o, Later.some, none)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideTargetNameQuery(oid),
        expected = guideTargetNameResult(none))
    }
  }

  test("changing observation duration invalidates name") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, ObsTime.some, none)
        _ <- setGuideTargetName(pi, o, targetName1.some)
        _ <- setObservationTimeAndDuration(pi, o, ObsTime.some, ObsDuration.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideTargetNameQuery(oid),
        expected = guideTargetNameResult(none))
    }
  }

}

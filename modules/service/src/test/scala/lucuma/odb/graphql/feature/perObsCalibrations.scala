// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.feature

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.Wavelength
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.graphql.subscription.SubscriptionUtils

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset

class perObsCalibrations
  extends OdbSuite
  with SubscriptionUtils
  with ExecutionTestSupport
  with ObservingModeSetupOperations {

  override val pi = TestUsers.Standard.pi(1, 101)
  val service     = TestUsers.service(3)

  val DefaultSnAt: Wavelength = Wavelength.fromIntNanometers(510).get

  override val validUsers = List(pi, service)

  val when = LocalDateTime.of(2024, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)

  case class GroupInfo(id: Group.Id, system: Boolean, name: String, calibrationRoles: List[CalibrationRole]) derives Decoder
  case class ObsInfo(id: Observation.Id, groupId: Option[Group.Id]) derives Decoder

  private def queryGroup(gid: Group.Id): IO[GroupInfo] =
    query(
      service,
      s"""query {
        group(groupId: "$gid") {
          id
          system
          name
          calibrationRoles
        }
      }"""
    ).flatMap { c =>
      c.hcursor.downField("group").as[GroupInfo]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  private def queryObservation(oid: Observation.Id): IO[ObsInfo] =
    query(
      service,
      s"""query {
        observation(observationId: "$oid") {
          id
          groupId
        }
      }"""
    ).flatMap { c =>
      c.hcursor.downField("observation").as[ObsInfo]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  test("F2 observation is automatically placed in system group with telluric role") {
    for {
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      oid  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      _    <- recalculateCalibrations(pid, when)
      obs  <- queryObservation(oid)
      grp  <- obs.groupId.traverse(queryGroup)
    } yield {
      assert(obs.groupId.isDefined)
      assert(grp.exists(_.system))
      assert(grp.exists(_.calibrationRoles.contains(CalibrationRole.Telluric)))
    }
  }

  test("Multiple F2 observations get separate system groups") {
    for {
      pid   <- createProgramAs(pi)
      tid1  <- createTargetWithProfileAs(pi, pid)
      tid2  <- createTargetWithProfileAs(pi, pid)
      oid1  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid1))
      oid2  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid2))
      _     <- recalculateCalibrations(pid, when)
      obs1  <- queryObservation(oid1)
      obs2  <- queryObservation(oid2)
    } yield {
      assert(obs1.groupId.isDefined)
      assert(obs2.groupId.isDefined)
      assert(obs1.groupId =!= obs2.groupId)
    }
  }
}

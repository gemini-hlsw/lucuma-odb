// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined.*
import lucuma.core.model.Group
import lucuma.core.model.User
import lucuma.odb.data.ObservingModeType
import lucuma.odb.service.CalibrationsService

class calibrations extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 101)
  val service  = TestUsers.service(3)

  val validUsers = List(pi, service)

  private def queryGroup(gid: Group.Id): IO[(Group.Id, Boolean, NonEmptyString)] =
    query(
      service,
      s"""query { group(groupId: "$gid") { id system name} }"""
    ).flatMap { c =>
      (for {
        id    <- c.hcursor.downField("group").downField("id").as[Group.Id]
        sys   <- c.hcursor.downField("group").downField("system").as[Boolean]
        name  <- c.hcursor.downField("group").downField("name").as[NonEmptyString]
      } yield (id, sys, name))
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
     }

  test("create group for calibrations") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, ObservingModeType.GmosNorthLongSlit.some)
      gr  <- groupElementsAs(pi, pid, None)
      _   <- withServices(service) { services =>
               services.session.transaction.use { xa =>
                 services.calibrationsService.recalculateCalibrations(pid)(using xa)
               }
             }
      gr1  <- groupElementsAs(pi, pid, None)
      cgid = gr1.lastOption.flatMap(_.swap.toOption)
      cg <- cgid.map(queryGroup).getOrElse(IO.raiseError(new RuntimeException("No calibration group")))
    } yield {
      assert(gr.size == 1)
      assert(gr1.size == 2)
      assert(cg._2)
      assertEquals(cg._3, CalibrationsService.CalibrationsGroupName)
    }
  }
}

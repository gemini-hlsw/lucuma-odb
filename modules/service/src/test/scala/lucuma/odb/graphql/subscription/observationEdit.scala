// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.syntax.show.*
import cats.syntax.traverse.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.Epoch
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.EditType
import lucuma.odb.data.Existence

class observationEdit extends OdbSuite with SubscriptionUtils {

  object Group1 {
    val pi       = TestUsers.Standard.pi(11, 110)
    val guest    = TestUsers.guest(12)
    val service  = TestUsers.service(13)
  }

  object Group2 {
    val pi       = TestUsers.Standard.pi(21, 210)
    val guest    = TestUsers.guest(22)
    val service  = TestUsers.service(23)
  }

  override def validUsers: List[User] =
    List(
      Group1.pi, Group1.guest, Group1.service,
      Group2.pi, Group2.guest, Group2.service
    )

  def subtitleSubscription(
    pid: Option[Program.Id],
    oid: Option[Observation.Id]
  ): String = {
    val args: String =
      (pid, oid) match {
        case (Some(p), Some(o)) => s"""(input: { programId: "${p.show}", observationId: "${o.show}" } )"""
        case (Some(p), None   ) => s"""(input: { programId: "${p.show}" } )"""
        case (None,    Some(o)) => s"""(input: { observationId: "${o.show}" } )"""
        case (None,    None   ) => ""
      }

    s"""
      subscription {
        observationEdit$args {
          editType
          value {
            subtitle
          }
        }
      }
    """
  }

  def subtitleObservationEdit(
    editType: EditType,
    subtitle: String
  ): Json =
    Json.obj(
      "observationEdit" -> Json.obj(
        "editType" -> Json.fromString(editType.tag.toUpperCase),
        "value"    -> Json.obj(
          "subtitle" -> Json.fromString(subtitle)
        )
      )
    )

  def subtitleCreated(subtitle: String): Json =
    subtitleObservationEdit(EditType.Created, subtitle)

  def subtitleUpdated(subtitle: String): Json =
    subtitleObservationEdit(EditType.Updated, subtitle)

  val titleSubscription =
    s"""
      subscription {
        observationEdit {
          observationId
          editType
          meta: value {
            existence
          }
          value {
            id
            title
          }
        }
      }
    """


  def deletedSubscription(pid: Program.Id) =
    s"""
      subscription {
        observationEdit(input: { programId: "${pid.show}" }) {
          observationId
          editType
          meta: value {
            existence
          }
          value {
            id
          }
        }
      }
    """

  def exploreSubscription(pid: Program.Id) =
    s"""
      subscription {
        observationEdit(input: { programId: "${pid.show}" }) {
          observationId
          editType
          meta: value {
            existence
          }
          value {
            id
            attachments {
              id
            }
          }
        }
      }
    """

  def exploreResponse(ref: Deferred[IO, Observation.Id]) =
    ref.get.map: oid =>
      Json.obj(
        "observationEdit" -> Json.obj(
          "observationId" -> Json.fromString(oid.show),
          "editType"      -> Json.fromString(EditType.Created.tag.toUpperCase),
          "meta"          -> Json.obj(
            "existence" -> Json.fromString(Existence.Present.tag.toUpperCase)
          ),
          "value" -> Json.obj(
            "id"             -> oid.asJson,
            "attachments" -> List.empty[Json].asJson
          )
        )
      )

  def titleUpdated(oid: Observation.Id, title: String): Json =
    Json.obj(
      "observationEdit" -> Json.obj(
        "observationId" -> Json.fromString(oid.show),
        "editType"      -> Json.fromString(EditType.Updated.tag.toUpperCase),
        "meta"          -> Json.obj(
          "existence" -> Json.fromString(Existence.Present.tag.toUpperCase)
        ),
        "value"         -> Json.obj(
          "id"    -> oid.asJson,
          "title" -> Json.fromString(title),
        )
      )
    )

  def calibrationDeleted(oid: Observation.Id): Json =
    Json.obj(
      "observationEdit" -> Json.obj(
        "observationId" -> oid.asJson,
        "editType" -> Json.fromString(EditType.HardDelete.tag.toUpperCase),
        "meta"     -> Json.Null,
        "value"    -> Json.Null
      )
    )

  def updateTargetName(user: User, tid: Target.Id, name: String) =
    sleep >>
      query(
        user = user,
        query = s"""
          mutation {
            updateTargets(input: {
              SET: { name: "$name" }
              WHERE: {
                id: { EQ: "$tid"}
              }
            }) {
              targets {
                id
                name
              }
            }
          }
        """
      ).void

  def updateTargetEpoch(user: User, tid: Target.Id, epoch: Epoch) =
    sleep >>
      query(
        user = user,
        query = s"""
          mutation {
            updateTargets(input: {
              SET: { sidereal: { epoch: "${Epoch.fromString.reverseGet(epoch)}" }}
              WHERE: {
                id: { EQ: "$tid" }
              }
            }) {
              targets {
                id
                name
              }
            }
          }
        """
      ).void

  def requirementsExposureTimeModeObservationEdit(etm: Json): Json =
    json"""
      {
        "observationEdit": {
          "editType": "UPDATED",
          "value": {
            "scienceRequirements": {
              "exposureTimeMode": $etm
            }
          }
        }
      }
    """

  val signalToNoiseObservationEdit: Json =
    val sn = json"""
      {
        "signalToNoise": {
          "value": 100.000,
          "at": {
            "picometers": 650000
          }
        },
        "timeAndCount": null
      }
    """
    requirementsExposureTimeModeObservationEdit(sn)

  val timeAndCountObservationEdit: Json =
    val tc = json"""
      {
        "signalToNoise": null,
        "timeAndCount": {
          "time": {
            "microseconds": 600000000
          },
          "count": 3,
          "at": {
            "picometers": 650000
          }
        }
      }
    """
    requirementsExposureTimeModeObservationEdit(tc)

  def updateRequirementsExposureTimeMode(user: User, oid: Observation.Id, etm: String): IO[Unit] =
    sleep >>
      query(
        user = user,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                scienceRequirements: {
                  exposureTimeMode: $etm
                }
              },
              WHERE: { id: { EQ: "$oid" } }
            }) {
              observations {
                id
              }
            }
          }
        """
      ).void
  
  def signalToNoiseUpdate(user: User, obsId: Observation.Id): IO[Unit] =
    val sn = s"""
      {
        signalToNoise: {
          value: 100,
          at: {
            picometers: 650000
          }
        }
      }
    """
    updateRequirementsExposureTimeMode(user, obsId, sn)

  def timeAndCountUpdate(user: User, obsId: Observation.Id): IO[Unit] =
    val tc = s"""
      {
        timeAndCount: {
          time: {
            microseconds: 600000000
          },
          count: 3,
          at: {
            picometers: 650000
          }
        }
      }
    """
    updateRequirementsExposureTimeMode(user, obsId, tc)
  
  val requirementsExposureTimeModeSubscription =
    s"""
      subscription {
        observationEdit {
          editType
          value {
            scienceRequirements {
              exposureTimeMode {
                signalToNoise {
                  value
                  at {
                    picometers
                  }
                }
                timeAndCount {
                  time {
                    microseconds
                  }
                  count
                  at {
                    picometers
                  }
                }
              }
            }
          }
        }
      }
    """

  test("trigger for my own new observations") {
    import Group1._
    List(pi, guest, service).traverse { user =>
      subscriptionExpect(
        user      = user,
        query     = subtitleSubscription(None, None),
        mutations =
          Right(
            createProgram(user, "foo").flatMap { pid =>
              createObservation(user, "foo subtitle 0", pid) >> createObservation(user, "foo subtitle 1", pid)
            }
          ),
        expected  = List(subtitleCreated("foo subtitle 0"), subtitleCreated("foo subtitle 1"))
      )
    }
  }

  test("trigger for new observations with an explore-like query") {
    import Group1._
    for {
      pid  <- createProgram(pi, "foo")
      ref0 <- Deferred[IO, Observation.Id]
      ref1 <- Deferred[IO, Observation.Id]
      _    <-
      subscriptionExpectF(
        user      = pi,
        query     = exploreSubscription(pid),
        mutations =
          Right(
              createObservation(pi, "foo subtitle 0", pid).flatMap(ref0.complete) >>
              createObservation(pi, "foo subtitle 1", pid).flatMap(ref1.complete)
          ),
        expectedF = List(ref0, ref1).traverse(exploreResponse)
      )
    } yield ()
  }

  test("trigger for my own new observations (but nobody else's) as guest user") {
    import Group2._
    subscriptionExpect(
      user      = guest,
      query     = subtitleSubscription(None, None),
      mutations =
        Right(
          createProgram(guest,   "foo").flatMap(createObservation(guest,   "foo subtitle", _)) >>
          createProgram(pi,      "bar").flatMap(createObservation(pi,      "bar subtitle", _)) >>
          createProgram(service, "baz").flatMap(createObservation(service, "baz subtitle", _))
        ),
      expected  = List(subtitleCreated("foo subtitle"))
    )
  }

  test("trigger for all observations as service user") {
    import Group2._
    subscriptionExpect(
      user      = service,
      query     = subtitleSubscription(None, None),
      mutations =
        Right(
          createProgram(guest,   "foo").flatMap(createObservation(guest,   "foo subtitle", _)) >>
          createProgram(pi,      "bar").flatMap(createObservation(pi,      "bar subtitle", _)) >>
          createProgram(service, "baz").flatMap(createObservation(service, "baz subtitle", _))
        ),
      expected  = List(subtitleCreated("foo subtitle"), subtitleCreated("bar subtitle"), subtitleCreated("baz subtitle"))
    )
  }

  test("trigger for one particular observation") {
    import Group1._

    for {
      pid  <- createProgram(pi, "foo")
      oid0 <- createObservation(pi, "obs 0", pid)
      oid1 <- createObservation(pi, "obs 1", pid)
      _    <- subscriptionExpect(
        user      = pi,
        query     = subtitleSubscription(None, Some(oid1)),
        mutations =
          Right(
            updateObservation(pi, "obs 0 - edit", oid0) >>
              updateObservation(pi, "obs 1 - edit", oid1)
          ),
        expected  = List(subtitleUpdated("obs 1 - edit"))
      )
    } yield ()
  }


  test("trigger for one particular program") {
    import Group1._

    for {
      pid0 <- createProgram(pi, "prog 0")
      pid1 <- createProgram(pi, "prog 1")
      _    <- subscriptionExpect(
        user      = pi,
        query     = subtitleSubscription(Some(pid0), None),
        mutations =
          Right(
            createObservation(pi, "prog 0 - edit", pid0) >>
              createObservation(pi, "obs 1 - edit", pid1)
          ),
        expected  = List(subtitleCreated("prog 0 - edit"))
      )
    } yield ()

  }

  test("work even if no database fields are selected") {
    import Group1.pi
    subscriptionExpect(
      user      = pi,
      query     = s"""
        subscription {
          observationEdit {
            editType
          }
        }
      """,
      mutations =
        Right(
          createProgram(pi, "foo").flatMap(createObservation(pi, "foo obs", _)).replicateA(2)
        ),
      expected = List.fill(2)(json"""{"observationEdit":{"editType":"CREATED"}}""")
    )
  }

  test("triggers for adding target to observation") {
    import Group1.pi

    for {
      pid  <- createProgram(pi, "foo")
      tid0 <- createTargetAs(pi, pid, "Zero")
      tid1 <- createTargetAs(pi, pid, "One")
      oid0 <- createObservationAs(pi, pid, None, tid0)
      oid1 <- createObservationAs(pi, pid)
      _    <- subscriptionExpect(
        user      = pi,
        query     = titleSubscription,
        mutations =
          Right(
            sleep >>
              updateAsterisms(
                pi,
                List(oid0, oid1),
                add = List(tid1),
                del = List.empty,
                exp = List((oid0, List(tid0, tid1)), (oid1, List(tid1)))
              )
          ),
        expected  = List(titleUpdated(oid1, "One"), titleUpdated(oid0, "Zero, One"))
      )
    } yield ()
  }

  test("triggers for deleting target from observation") {
    import Group1.pi

    for {
      pid  <- createProgram(pi, "foo")
      tid0 <- createTargetAs(pi, pid, "Zero")
      tid1 <- createTargetAs(pi, pid, "One")
      oid0 <- createObservationAs(pi, pid, None, tid0, tid1)
      oid1 <- createObservationAs(pi, pid, None, tid0)
      _    <- subscriptionExpect(
        user      = pi,
        query     = titleSubscription,
        mutations =
          Right(
            sleep >>
              updateAsterisms(
                pi,
                List(oid0, oid1),
                add = List.empty,
                del = List(tid0),
                exp = List((oid0, List(tid1)), (oid1, List.empty))
              )
          ),
        expected  = List(titleUpdated(oid0, "One"), titleUpdated(oid1, "Untargeted"))
      )
    } yield ()
  }

  test("triggers for changing target name in asterism") {
    import Group1.pi

    for {
      pid  <- createProgram(pi, "foo")
      tid0 <- createTargetAs(pi, pid, "Zero")
      tid1 <- createTargetAs(pi, pid, "One")
      oid0 <- createObservationAs(pi, pid, None, tid0, tid1)
      oid1 <- createObservationAs(pi, pid, None, tid0)
      _    <- subscriptionExpect(
        user      = pi,
        query     = titleSubscription,
        mutations =
          Right(updateTargetName(pi, tid0, "New Name")),
        expected  = List(titleUpdated(oid0, "New Name, One"), titleUpdated(oid1, "New Name"))
      )
    } yield ()
  }

  test("triggers for changing target epoch in asterism") {
    import Group1.pi

    for {
      pid  <- createProgram(pi, "foo")
      tid0 <- createTargetAs(pi, pid, "Zero")
      tid1 <- createTargetAs(pi, pid, "One")
      oid0 <- createObservationAs(pi, pid, None, tid0, tid1)
      oid1 <- createObservationAs(pi, pid, None, tid0)
      _    <- subscriptionExpect(
        user      = pi,
        query     = titleSubscription,
        mutations =
          Right(updateTargetEpoch(pi, tid0, Epoch.B1950)),
        expected  = List(titleUpdated(oid0, "Zero, One"), titleUpdated(oid1, "Zero"))
      )
    } yield ()
  }

  test("triggers for changing target epoch in blind offset") {
    import Group1.pi

    for {
      pid  <- createProgram(pi, "foo")
      tid0 <- createTargetAs(pi, pid, "Zero")
      tid1 <- createTargetAs(pi, pid, "One")
      (oid, btid) <- createObservationWithBlindOffsetAs(pi, pid, "Blind offset", tid0, tid1)
      _    <- subscriptionExpect(
        user      = pi,
        query     = titleSubscription,
        mutations =
          Right(updateTargetEpoch(pi, btid, Epoch.B1950)),
        expected  = List(titleUpdated(oid, "Zero, One"))
      )
    } yield ()
  }

  test("triggers for deleting a calibration observation") {
    import Group1.{ pi, service }
    def deleteCalibrationObservation(oid: Observation.Id) =
      withServices(service) { services =>
        services.session.transaction.use { xa =>
          services.observationService.deleteCalibrationObservations(NonEmptyList.one(oid))(using xa)
        }
      }

    for {
      pid  <- createProgram(pi, "foo")
      tid0 <- createTargetAs(pi, pid, "Zero")
      // An observation with a single target is essentially a calib observation
      oid  <- createObservationAs(pi, pid, None, tid0)
      _    <- setObservationCalibratioRole(oid, Some(CalibrationRole.Telluric))
      _    <- subscriptionExpect(
        user      = pi,
        query     = deletedSubscription(pid),
        mutations =
          Right(deleteCalibrationObservation(oid)),
        expected  = List(
          json"""
            {
              "observationEdit" : {
                "observationId" : $oid,
                "editType" : "UPDATED",
                "meta" : null,
                "value" : null
              }
            }
          """,
          calibrationDeleted(oid)
        )
      )
    } yield ()
  }

  test("triggers for adding/editing/deleting science requirements exposure time mode") {
    import Group1.pi
    for {
      pid <- createProgram(pi, "foo")
      oid <- createObservationAs(pi, pid)
      _   <- subscriptionExpect(
        user      = pi,
        query     = requirementsExposureTimeModeSubscription,
        mutations = Right(
          signalToNoiseUpdate(pi, oid) >>
          timeAndCountUpdate(pi, oid) >>
          updateRequirementsExposureTimeMode(pi, oid, "null")),
        expected  = List(
          signalToNoiseObservationEdit,
          timeAndCountObservationEdit,
          requirementsExposureTimeModeObservationEdit(Json.Null))
      )
    } yield ()
  }
}

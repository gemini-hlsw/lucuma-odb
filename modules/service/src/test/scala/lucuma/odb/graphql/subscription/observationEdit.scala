// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Site
import lucuma.core.enums.Site.GN
import lucuma.core.enums.Site.GS
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.EditType

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

  def observationEdit(
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

  def created(subtitle: String): Json =
    observationEdit(EditType.Created, subtitle)

  def updated(subtitle: String): Json =
    observationEdit(EditType.Updated, subtitle)

  test("trigger for my own new observations".ignore) {
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
        expected  = List(created("foo subtitle 0"), created("foo subtitle 1"))
      )
    }
  }

  test("trigger for my own new observations (but nobody else's) as guest user".ignore) {
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
      expected  = List(created("foo subtitle"))
    )
  }

  test("trigger for all observations as service user".ignore) {
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
      expected  = List(created("foo subtitle"), created("bar subtitle"), created("baz subtitle"))
    )
  }

  test("trigger for one particular observation".ignore) {
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
            updateObservation(pi, "obs 0 - edit", pid, oid0) >>
              updateObservation(pi, "obs 1 - edit", pid, oid1)
          ),
        expected  = List(updated("obs 1 - edit"))
      )
    } yield ()
  }


  test("trigger for one particular program".ignore) {
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
        expected  = List(created("prog 0 - edit"))
      )
    } yield ()

  }

  test("work even if no database fields are selected".ignore) {
    import Group1.pi
    subscriptionExpect(
      user      = pi,
      query     = s"""
        subscription {
          observationEdit {
            editType
            id
          }
        }
      """,
      mutations =
        Right(
          createProgram(pi, "foo").flatMap(createObservation(pi, "foo obs", _)).replicateA(2)
        ),
      expected = List.fill(2)(json"""{"observationEdit":{"editType":"CREATED","id":0}}""")
    )
  }

  
  test("work for edits to gmos longslit config") {
    import Group1.pi

    def grating(site: Site) =
      site match
        case GN => "B1200_G5301"
        case GS => "B1200_G5321"
      
    val setup: IO[List[(Program.Id, Site, Observation.Id)]] = 
      createProgramAs(pi).flatMap { pid =>      
        Site.all.traverse { site =>
          createObsWithGmosLongSlitObservingModeAs(pi, pid, site, grating(site)).map((pid, site, _))
        }
      }

    def update(pid: Program.Id, site: Site, oid: Observation.Id): String =
      s"""
          mutation {
            updateObservations(input: {
              programId: ${pid.asJson}
              SET: {
                observingMode: {
                  gmos${siteName(site)}LongSlit: {
                    centralWavelength: {
                      nanometers: 4.3
                    }
                  }
                }
              },
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              observations {
                id
              }
            }
          }
        """

    def expect(oid: Observation.Id): Json =
      json"""{
        "observationEdit" : {
          "editType" : "UPDATED",
          "value" : {
            "id" : $oid
          }
        }
      }"""

    setup.flatMap { tuples =>
      subscriptionExpect(
        pi,
        query = """
          subscription {
            observationEdit {
              editType
              value {
                id
              }
            }
          }
        """,
        mutations = Left(tuples.map(update).tupleRight(None)),
        expected = tuples.map(_._3).map(expect)
      )
    }

  }

}

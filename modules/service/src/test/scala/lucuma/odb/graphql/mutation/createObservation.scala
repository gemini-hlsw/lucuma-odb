// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.model.Observation
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.graphql.OdbSuite

class createObservation extends OdbSuite with CreateProgramOps with LinkUserOps with SetAllocationOps with CreateObservationOps {

  val pi       = TestUsers.Standard.pi(nextId, nextId)
  val pi2      = TestUsers.Standard.pi(nextId, nextId)
  val pi3      = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.Ca)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val guest    = TestUsers.guest(nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi, pi2, pi3, ngo, staff, admin, guest, service)

  def createUsers(users: User*): IO[Unit] =
    users.toList.traverse_(createProgramAs) // TODO: something cheaper

  test("[general] default name should be null") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
            }) {
              observation {
                program {
                  id
                }
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("program")
          .downField("id")
          .as[Program.Id]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, pid)
      }
    }
  }

  test("[general] subtitle can't be empty") {
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL("Argument 'input.SET.subtitle' is invalid: string value must be non-empty.") {
        query(pi,
          s"""
            mutation {
              createObservation(input: {
                programId: ${pid.asJson}
                SET: {
                  subtitle: ""
                }
              }) {
                observation {
                  subtitle
                }
              }
            }
            """
        )
      }
    }
  }

  test("[general] created observation should have specified program as parent") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
            }) {
              observation {
                program {
                  id
                }
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("program")
          .downField("id")
          .as[Program.Id]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, pid)
      }
    }
  }

  test("[general] created observation should have specified subtitle (non-null)") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                subtitle: "crunchy frog"
              }
            }) {
              observation {
                subtitle
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("subtitle")
          .as[String]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, "crunchy frog")
      }
    }
  }

  test("[general] created observation should have specified subtitle (null)") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                subtitle: null
              }
            }) {
              observation {
                subtitle
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("subtitle")
          .as[Option[String]]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, None)
      }
    }
  }

  test("[general] created observation should have specified status") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                status: FOR_REVIEW
              }
            }) {
              observation {
                status
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("status")
          .as[ObsStatus]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, ObsStatus.ForReview)
      }
    }
  }

  test("[general] created observation should have specified active status") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                activeStatus: INACTIVE
              }
            }) {
              observation {
                activeStatus
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("activeStatus")
          .as[ObsActiveStatus]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, ObsActiveStatus.Inactive)
      }
    }
  }

  test("[pi] pi can create an observation in their own program") {
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid)
    }
  }

  test("[pi] pi can't create an observation in someone else's program") {
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${pi2.id} is not authorized to perform this action") {
        createObservationAs(pi2, pid)
      }
    }
  }

  // TODO: more access control tests

}

trait CreateObservationOps { this: OdbSuite =>

  def createObservationAs(
    user: User,
    pid: Program.Id,
  ): IO[Observation.Id] =
    query(user, s"mutation { createObservation(input: { programId: ${pid.asJson} }) { observation { id } } }").flatMap { js =>
      js.hcursor
        .downField("createObservation")
        .downField("observation")
        .downField("id")
        .as[Observation.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

}
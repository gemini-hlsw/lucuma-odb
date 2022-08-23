// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.Json
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.enums.CloudExtinction
import lucuma.core.model.Observation
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.graphql.input.CoordinatesInput

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

  test("[general] created observation has no explicit base by default") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
          }) {
            observation {
              targetEnvironment {
                explicitBase {
                  ra { hms }
                  dec { dms }
                }
              }
            }
          }
        }
        """).map { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("targetEnvironment")
          .downField("explicitBase")
          .downField("ra")
          .failed
        assert(get, "Expected a failed cursor on ra")
      }
    }
  }

  test("[general] created observation should have specified explicit base") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson}
          SET: {
            targetEnvironment: {
              explicitBase: {
                ra: { hms: "1:00:00" }
                dec: { dms: "2:00:00" }
              }
            }
          }
        }) {
          observation {
            targetEnvironment {
              explicitBase {
                ra { hours }
                dec { degrees }
              }
            }
          }
        }
      }
      """).flatMap { js =>
        val c = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("targetEnvironment")
          .downField("explicitBase")

        val ra = c
          .downField("ra")
          .downField("hours")
          .as[Int]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]

        val dec = c
          .downField("dec")
          .downField("degrees")
          .as[Int]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]

        for {
          _ <- assertIO(ra, 1)
          _ <- assertIO(dec, 2)
        } yield ()
      }
    }
  }

  test("[general] both ra and dec are required to set an explicit base") {
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(CoordinatesInput.messages.BothRaAndDecNeeded) {
        query(pi,
          s"""
            mutation {
              createObservation(input: {
                programId: ${pid.asJson}
                SET: {
                  targetEnvironment: {
                    explicitBase: {
                      ra: { hms: "1:00:00" }
                    }
                  }
                }
              }) {
                observation {
                  targetEnvironment {
                    explicitBase {
                      ra { hours }
                    }
                  }
                }
              }
            }
            """
        )
      }
    }
  }

  test("[general] created observation should have specified cloud extinction") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                constraintSet: {
                  cloudExtinction: ONE_POINT_FIVE
                }
              }
            }) {
              observation {
                constraintSet {
                  cloudExtinction
                }
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("constraintSet")
          .downField("cloudExtinction")
          .as[CloudExtinction]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, CloudExtinction.OnePointFive)
      }
    }
  }

  test("[general] created observation can default cloud extinction") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
            }) {
              observation {
                constraintSet {
                  cloudExtinction
                }
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("constraintSet")
          .downField("cloudExtinction")
          .as[CloudExtinction]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, CloudExtinction.ThreePointZero)
      }
    }
  }

  test("[general] created observation should have specified air mass") {
    createProgramAs(pi).flatMap { pid =>
      query(pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                constraintSet: {
                  elevationRange: {
                    airMass: {
                      min: "1.2"
                      max: "1.3"
                    }
                  }
                }
              }
            }) {
              observation {
                constraintSet {
                  elevationRange {
                    airMass {
                      min
                    }
                  }
                }
              }
            }
          }
          """).flatMap { js =>
        val get = js.hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("constraintSet")
          .downField("elevationRange")
          .downField("airMass")
          .downField("min")
          .as[BigDecimal]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
        assertIO(get, BigDecimal("1.2"))
      }
    }
  }

  test("[general] created observation should have specified asterism") {

    def createObs(pid: Program.Id, t0: Target.Id, t1: Target.Id): IO[List[Target.Id]] =
      query(
        pi,
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [ ${t0.asJson}, ${t1.asJson} ]
                }
              }
            }) {
              observation {
                targetEnvironment {
                  asterism {
                    id
                  }
                }
              }
            }
          }
        """.stripMargin
      ).map { js =>
        js
          .hcursor
          .downField("createObservation")
          .downField("observation")
          .downField("targetEnvironment")
          .downField("asterism")
          .values
          .toList // List[Iterable[Json]]
          .flatMap { it =>
            it.toList.flatMap { targetJson =>
              targetJson.hcursor.downField("id").as[Target.Id].toOption.toList
            }
          }
      }

    for {
      pid <- createProgramAs(pi)
      t0  <- createEmptyTargetAs(pi, pid, "Biff")
      t1  <- createEmptyTargetAs(pi, pid, "Henderson")
      res <- createObs(pid, t0, t1)
    } yield assertEquals(res, List(t0, t1))
  }

  test("[general] handle unknown target id") {

    def createObs(pid: Program.Id): IO[Unit] =
      interceptGraphQL("foo")(
        query(
          pi,
          s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [ ${Target.Id.fromLong(1).get.asJson} ]
                }
              }
            }) {
              observation {
                targetEnvironment {
                  asterism {
                    id
                  }
                }
              }
            }
          }
        """.stripMargin
        )
      )

    for {
      pid <- createProgramAs(pi)
      _   <- createObs(pid)
    } yield ()
  }

  test("[pi] pi can create an observation in their own program") {
    createProgramAs(pi).flatMap { pid =>
      createObservationAs(pi, pid)
    }
  }

  test("[pi] pi can't create an observation in someone else's program") {
    createProgramAs(pi).flatMap { pid =>
      interceptGraphQL(s"User ${pi2.id} is not authorized to perform this action.") {
        createObservationAs(pi2, pid)
      }
    }
  }

  // TODO: more access control tests

}

trait CreateObservationOps { this: OdbSuite =>

  def createObservationAs(
    user: User,
    pid: Program.Id
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

  def createEmptyTargetAs(
    user: User,
    pid:  Program.Id,
    name: String
  ): IO[Target.Id] =
    query(
      user,
      s"""
        mutation {
          createTarget(
            input: {
              programId: ${pid.asJson}
              SET: {
                name: "$name"
                sidereal: {
                  ra: { hours: "0.0" }
                  dec: { degrees: "0.0" }
                }
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: B5_III
                      }
                      brightnesses: []
                    }
                  }
                }
              }
            }
          ) {
            target { id }
          }
        }
      """
    ).flatMap { js =>
      js.hcursor
        .downField("createTarget")
        .downField("target")
        .downField("id")
        .as[Target.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

}
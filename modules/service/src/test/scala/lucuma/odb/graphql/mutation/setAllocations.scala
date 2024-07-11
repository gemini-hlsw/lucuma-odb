// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.Partner
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.ScienceBand
import lucuma.odb.graphql.input.AllocationInput

class setAllocations extends OdbSuite {

  val guest    = TestUsers.guest(nextId)
  val pi       = TestUsers.Standard.pi(nextId, nextId)
  val ngo      = TestUsers.Standard.ngo(nextId, nextId, Partner.CA)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi, ngo, staff, admin, guest, service)

  test("guest, pi, ngo can't set allocation") {
    List(guest, pi, ngo).traverse { user =>
      createProgramAs(user).flatMap { pid =>
        interceptGraphQL(s"User ${user.id} is not authorized to perform this operation.") {
          setOneAllocationAs(user, pid, Partner.CA, ScienceBand.Band1, 42.hourTimeSpan)
        }
      }
    }
  }

  test("admin, staff, service can set (and update) allocation in any program") {
    createProgramAs(pi).flatMap { pid =>
      List((admin, 2L), (staff, 3L), (service, 4L)).traverse { case (user, hours) =>
        setOneAllocationAs(user, pid, Partner.US, ScienceBand.Band1, TimeSpan.fromHours(hours).get)
      }
    }
  }

  test("should be able to read allocations back") {
    val allocations = List(
      AllocationInput(Partner.US, ScienceBand.Band2, TimeSpan.fromHours(1.23).get),
      AllocationInput(Partner.CA, ScienceBand.Band3, TimeSpan.fromHours(4.56).get)
    )
    createProgramAs(pi).flatMap { pid =>
      setAllocationsAs(staff, pid, allocations) *>
      expect(
        user = pi,
        query = s"""
          query {
            program(programId: "$pid") {
              allocations {
                partner
                scienceBand
                duration {
                  hours
                }              
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "program" : {
                "allocations" : [
                  {
                    "partner" : "US",
                    "scienceBand": "BAND2",
                    "duration" : {
                      "hours" : 1.230000
                    }
                  },
                  {
                    "partner" : "CA",
                    "scienceBand": "BAND3",
                    "duration" : {
                      "hours" : 4.560000
                    }
                  }
                ]
              }
            }
          """
        )
      )
    }
  }

  test("should refuse duplicate entries") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = staff,
        query = s"""
          mutation {
            setAllocations(input: {
              programId:   "$pid"
              allocations: [
                {
                  partner: US
                  scienceBand: BAND2
                  duration: { hours: "1.23" }
                },
                {
                  partner: US
                  scienceBand: BAND2
                  duration: { hours: "4.56" }
                }
              ]
            }) {
              allocations { partner }
            }
          }
        """,
        expected = List("Argument 'input' is invalid: Each partner + band combination may only appear once.").asLeft
      )
    }
  }

  def getBand(user: User, oid: Observation.Id): IO[Option[ScienceBand]] =
    query(
      user = user,
      query = s"""
        query {
          observation(
            observationId: "$oid"
          ) {
            scienceBand
          }
        }
      """
    ).flatMap { json =>
      json.hcursor
          .downFields("observation", "scienceBand")
          .as[Option[ScienceBand]]
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]
    }

  test("single band allocation sets observation band") {
    val allocations = List(
      AllocationInput(Partner.US, ScienceBand.Band2, TimeSpan.fromHours(1.23).get),
      AllocationInput(Partner.CA, ScienceBand.Band2, TimeSpan.fromHours(4.56).get)
    )
    val band = for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- setAllocationsAs(staff, pid, allocations)
      b   <- getBand(pi, oid)
    } yield b
    assertIO(band, ScienceBand.Band2.some)
  }

  test("empty allocation does not set observation band") {
    val band = for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- setAllocationsAs(staff, pid, List.empty)
      b   <- getBand(pi, oid)
    } yield b
    assertIO(band, none[ScienceBand])
  }

  test("multiple band allocation does not set observation band") {
    val allocations = List(
      AllocationInput(Partner.US, ScienceBand.Band1, TimeSpan.fromHours(1.23).get),
      AllocationInput(Partner.CA, ScienceBand.Band2, TimeSpan.fromHours(4.56).get)
    )
    val band = for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _   <- setAllocationsAs(staff, pid, allocations)
      b   <- getBand(pi, oid)
    } yield b
    assertIO(band, none[ScienceBand])
  }
}

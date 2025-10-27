// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.DateInterval
import lucuma.odb.data.Existence

import java.time.LocalDate
import java.time.Month

class updatePrograms extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 101)
  val ngo      = TestUsers.Standard.ngo(2, 102, Partner.CA)
  val staff    = TestUsers.Standard.staff(3, 103)
  val admin    = TestUsers.Standard.admin(4, 104)
  val guest    = TestUsers.guest(5)
  val service  = TestUsers.service(6)

  val validUsers = List(pi, ngo, staff, admin, guest, service).toList

  test("edit name") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  name: "new name"
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              hasMore
              programs {
                id
                name
              }
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": {
              "hasMore": false,
              "programs": [
                {
                  "id": $pid,
                  "name": "new name"
                }
              ]
            }
          }
          """
        )
      ) >> chronProgramUpdates(pid).map(_.drop(1)).assertEquals(
        List(
          json"""
          {
            "c_user"                : ${pi.id},
            "c_mod_name"            : true,
            "c_new_name"            : "new name",
            "c_operation"           : "UPDATE",
            "c_program_id"          : $pid,
            "c_mod_existence"       : false,
            "c_new_existence"       : null,
            "c_mod_program_id"      : false,
            "c_new_program_id"      : null,
            "c_mod_proposal_status" : false,
            "c_new_proposal_status" : null
          }
          """
        )
      )
    }
  }

  test("delete name"):
    createProgramAs(pi, "Foo").flatMap: pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  name: null
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              hasMore
              programs {
                id
                name
              }
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": {
              "hasMore": false,
              "programs": [
                {
                  "id": $pid,
                  "name": null
                }
              ]
            }
          }
          """
        )
      )

  test("edit description"):
    createProgramAs(pi, "Foo").flatMap: pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  description: "Foo"
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              hasMore
              programs {
                id
                description
              }
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": {
              "hasMore": false,
              "programs": [
                {
                  "id": $pid,
                  "description": "Foo"
                }
              ]
            }
          }
          """
        )
      )

  test("edit existence") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  existence: DELETED
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
                includeDeleted: true
              }
            ) {
              programs {
                id
                existence
              }
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": {
              "programs": [
                {
                  "id": $pid,
                  "existence": ${Existence.Deleted:Existence}
                }
              ]
            }
          }
          """
        )
      ) >> chronProgramUpdates(pid).map(_.drop(1)).assertEquals(
        List(
          json"""
          {
            "c_user"                : ${pi.id},
            "c_mod_name"            : false,
            "c_new_name"            : null,
            "c_operation"           : "UPDATE",
            "c_program_id"          : $pid,
            "c_mod_existence"       : true,
            "c_new_existence"       : "deleted",
            "c_mod_program_id"      : false,
            "c_new_program_id"      : null,
            "c_mod_proposal_status" : false,
            "c_new_proposal_status" : null
          }
          """
        )
      )
    }
  }

  test("bulk update basic properties") {
    // create a bunch and edit a few of them
    createProgramAs(pi).replicateA(10).flatMap { pids =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  name: "updated"
                }
                WHERE: {
                  id: {
                    IN: [ ${pids.take(3).mkString("\"", "\", \"", "\"")} ]
                  }
                }
              }
            ) {
              programs {
                id
                name
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : ${pids(0)},
                    "name" : "updated"
                  },
                  {
                    "id" : ${pids(1)},
                    "name" : "updated"
                  },
                  {
                    "id" : ${pids(2)},
                    "name" : "updated"
                  }
                ]
              }
            }
          """
        )
      )

    }
  }

  def editProprietaryMonthsQuery(pid: Program.Id): String =
    s"""
      mutation {
        updatePrograms(
          input: {
            SET: {
              goa: { proprietaryMonths: 42 }
            }
            WHERE: {
              id: {
                EQ: "$pid"
              }
            }
          }
        ) {
          programs {
            id
            goa { proprietaryMonths }
          }
        }
      }
    """

  test("can edit proprietaryMonths as staff") {
    createProgramAs(pi).flatMap: pid =>
      expect(
        user = staff,
        query = editProprietaryMonthsQuery(pid),
        expected =
          json"""
          {
            "updatePrograms": {
              "programs": [
                {
                  "id": $pid,
                  "goa": { "proprietaryMonths": 42 }
                }
              ]
            }
          }
          """.asRight
      )
  }

  test("can edit other GOA properties as pi") {
    createProgramAs(pi).flatMap: pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  goa: {
                    shouldNotify: false,
                    privateHeader: true
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              programs {
                id
                goa {
                  shouldNotify
                  privateHeader
                }
              }
            }
          }
        """,
        expected =
          json"""
          {
            "updatePrograms": {
              "programs": [
                {
                  "id": $pid,
                  "goa": {
                    "shouldNotify": false,
                    "privateHeader": true
                  }
                }
              ]
            }
          }
          """.asRight
      )
  }

  test("cannot edit proprietaryMonths as pi") {
    createProgramAs(pi).flatMap: pid =>
      expect(
        user = pi,
        query = editProprietaryMonthsQuery(pid),
        expected = List(
          "Only staff may set the proprietary months."
        ).asLeft
      )
  }

  def editActivePeriodQuery(pid: Program.Id): String =
    s"""
      mutation {
        updatePrograms(
          input: {
            SET: {
              activeStart: "2020-01-01"
              activeEnd: "2030-01-01"
            }
            WHERE: {
              id: {
                EQ: "$pid"
              }
            }
          }
        ) {
          programs {
            id
            active {
              start
              end
            }
          }
        }
      }
    """

  test("can edit active period as staff"):
    createProgramAs(pi).flatMap: pid =>
      expect(
        user  = staff,
        query = editActivePeriodQuery(pid),
        expected =
          json"""
          {
            "updatePrograms": {
              "programs": [
                {
                  "id": $pid,
                  "active": {
                     "start": "2020-01-01",
                     "end": "2030-01-01"
                  }
                }
              ]
            }
          }
          """.asRight
      )

  test("changing CfP active period in non-default active period has no impact"):
    createCallForProposalsAs(staff, CallForProposalsType.RegularSemester).flatMap { cid =>
      createProgramAs(pi).flatMap { pid =>
        for
          _ <- addProposal(pi, pid, cid.some).void
          _ <- assertIOBoolean(getActivePeriod(pi, pid).map(_ ===  DateInterval.between(LocalDate.of(2025, Month.FEBRUARY, 1), LocalDate.of(2025, Month.JULY, 31))))
          _ <- query(staff, editActivePeriodQuery(pid)).void
          _ <- assertIOBoolean(getActivePeriod(pi, pid).map(_ ===  DateInterval.between(LocalDate.of(2020, Month.JANUARY, 1), LocalDate.of(2030, Month.JANUARY, 1))))
          _ <- query(staff,
            s"""
              mutation {
                updateCallsForProposals(input: {
                  SET: {
                    activeStart: "2024-12-31"
                    activeEnd:   "2026-01-01"
                  },
                  WHERE: {
                    id: { EQ: "$cid" }
                  }
                }) {
                  callsForProposals {
                    active {
                      start
                      end
                    }
                  }
                }
              }
            """
          )
          _ <- assertIOBoolean(getActivePeriod(pi, pid).map(_ ===  DateInterval.between(LocalDate.of(2020, Month.JANUARY, 1), LocalDate.of(2030, Month.JANUARY, 1))))
        yield ()
      }
    }
}

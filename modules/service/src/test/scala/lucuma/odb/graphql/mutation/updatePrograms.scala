// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import lucuma.core.math.Offset.P
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.graphql.OdbSuite

class updatePrograms extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 101)
  val ngo      = TestUsers.Standard.ngo(2, 102, Partner.Ca)
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
            "c_user"              : ${pi.id},
            "c_mod_name"          : true,
            "c_new_name"          : "new name",
            "c_operation"         : "UPDATE",
            "c_mod_pts_pi"        : false,
            "c_new_pts_pi"        : null,
            "c_program_id"        : $pid,
            "c_mod_existence"     : false,
            "c_new_existence"     : null,
            "c_mod_pi_user_id"    : false,
            "c_mod_program_id"    : false,
            "c_new_pi_user_id"    : null,
            "c_new_program_id"    : null,
            "c_mod_pi_user_type"  : false,
            "c_new_pi_user_type"  : null,
            "c_mod_pts_execution" : false,
            "c_mod_pts_uncharged" : false,
            "c_new_pts_execution" : null,
            "c_new_pts_uncharged" : null
          }
          """
        )
      )
    }
  }

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
            "c_user"              : ${pi.id},
            "c_mod_name"          : false,
            "c_new_name"          : null,
            "c_operation"         : "UPDATE",
            "c_mod_pts_pi"        : false,
            "c_new_pts_pi"        : null,
            "c_program_id"        : $pid,
            "c_mod_existence"     : true,
            "c_new_existence"     : "deleted",
            "c_mod_pi_user_id"    : false,
            "c_mod_program_id"    : false,
            "c_new_pi_user_id"    : null,
            "c_new_program_id"    : null,
            "c_mod_pi_user_type"  : false,
            "c_new_pi_user_type"  : null,
            "c_mod_pts_execution" : false,
            "c_mod_pts_uncharged" : false,
            "c_new_pts_execution" : null,
            "c_new_pts_uncharged" : null
          }
          """
        )
      )
    }
  }


  test("edit proposal (attempt add w/ insufficient information)") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    title: "new title"
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
                proposal {
                  title
                }
              }
            }
          }
        """,
        expected =
          Left(List("One or more programs has no proposal, and there is insufficient information to create one. To add a proposal all required fields must be specified."))
      )
    }
  }

  test("edit proposal (add)") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    title: "new title"
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    category: COSMOLOGY
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
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
                proposal {
                  title
                  proposalClass {
                    ... on Queue {
                      minPercentTime
                    }
                  }
                  category
                  toOActivation
                  partnerSplits {
                    partner
                    percent
                  }
                }
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid,
                    "proposal" : {
                      "title" : "new title",
                      "proposalClass" : {
                        "minPercentTime" : 50
                      },
                      "category" : "COSMOLOGY",
                      "toOActivation" : "NONE",
                      "partnerSplits" : [
                        {
                          "partner" : "US",
                          "percent" : 100
                        }
                      ]
                    }
                  }
                ]
              }
            }
          """)
      )
    }
  }

  test("edit proposal (non-class properties)") {
    createProgramAs(pi).flatMap { pid =>
      // First add the proposal
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    category: COSMOLOGY
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
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
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid
                  }
                ]
              }
            }
          """)
      ) >>
      // Now update it with a different type-A proposal class
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    title: "updated title"
                    category: SMALL_BODIES
                    toOActivation: RAPID
                    partnerSplits: [
                      {
                        partner: AR
                        percent: 70
                      }
                      {
                        partner: KECK
                        percent: 30
                      }
                    ]
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
                proposal {
                  title
                  category
                  toOActivation
                  partnerSplits {
                    partner
                    percent
                  }
                }
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid,
                    "proposal" : {
                      "title" : "updated title",
                      "category" : "SMALL_BODIES",
                      "toOActivation" : "RAPID",
                      "partnerSplits" : [
                        {
                          "partner" : "AR",
                          "percent" : 70
                        },
                        {
                          "partner" : "KECK",
                          "percent" : 30
                        }
                      ]
                    }
                  }
                ]
              }
            }
          """)
      )

    }
  }

  test("edit proposal (proposal class, type A -> type A)") {
    createProgramAs(pi).flatMap { pid =>

      // First add the proposal
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
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
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid
                  }
                ]
              }
            }
          """)
      ) >>
      // Now update it with a different type-A proposal class
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      classical: {
                        minPercentTime: 40
                      }
                    }
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
                proposal {
                  proposalClass {
                    ... on Classical {
                      minPercentTime
                    }
                  }
                }
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid,
                    "proposal" : {
                      "proposalClass" : {
                        "minPercentTime" : 40
                      }
                    }
                  }
                ]
              }
            }
          """)
      )

    }

  }

  test("edit proposal (proposal class, type A -> type B, incomplete)") {
    createProgramAs(pi).flatMap { pid =>
      // First add the proposal
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
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
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid
                  }
                ]
              }
            }
          """)
      ) >>
      // Now update it with an incomplete type-B proposal class
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      intensive: {
                        minPercentTime: 40
                      }
                    }
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
                proposal {
                  proposalClass {
                    ... on Intensive {
                      minPercentTime
                    }
                  }
                }
              }
            }
          }
        """,
        expected = Left(List(
          "The specified edits for proposal class do not match the proposal class for one or more specified programs' proposals. To change the proposal class you must specify all fields for that class."
        ))
      )

    }

  }

  test("edit proposal (proposal class, type A -> type B)") {
    createProgramAs(pi).flatMap { pid =>
      // First add the proposal
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
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
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : {
                "programs": [
                  {
                    "id" : $pid
                  }
                ]
              }
            }
          """)
      ) >>
      // Now update it with an valid type-B proposal class
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      intensive: {
                        minPercentTime: 40
                        minPercentTotalTime: 10
                        totalTime: {
                          hours: 10.5
                        }
                      }
                    }
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
                proposal {
                  proposalClass {
                    ... on Intensive {
                      minPercentTime
                      minPercentTotalTime
                      totalTime {
                        hours
                        iso
                      }
                    }
                  }
                }
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
                    "id" : $pid,
                    "proposal" : {
                      "proposalClass" : {
                        "minPercentTime" : 40,
                        "minPercentTotalTime" : 10,
                        "totalTime" : {
                          "hours" : 10.500000,
                          "iso" : "PT10H30M"
                        }
                      }
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

  test("bulk update proposal: one insert, one update") {
    (createProgramAs(pi), createProgramAs(pi)).tupled.flatMap { (pid1, pid2) =>
      // Add a proposal to one of them
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid1"
                  }
                }
              }
            ) {
              programs {
                id
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
                    "id" : $pid1
                  }
                ]
              }
            }
          """
        )
      )
      // Now update both of them
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    proposalClass: {
                      classical: {
                        minPercentTime: 30
                      }
                    }
                    toOActivation: RAPID
                    partnerSplits: [
                      {
                        partner: KECK
                        percent: 100
                      }
                    ]
                  }
                }
                WHERE: {
                  id: {
                    IN: ["$pid1", "$pid2"]
                  }
                }
              }
            ) {
              programs {
                id
                proposal {
                  proposalClass {
                    ... on Classical {
                      minPercentTime
                    }
                  }
                  partnerSplits {
                    partner
                    percent
                  }
                }
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
                    "id" : $pid1,
                    "proposal" : {
                      "proposalClass" : {
                        "minPercentTime" : 30
                      },
                      "partnerSplits" : [
                        {
                          "partner" : "KECK",
                          "percent" : 100
                        }
                      ]
                    }
                  },
                  {
                    "id" : $pid2,
                    "proposal" : {
                      "proposalClass" : {
                        "minPercentTime" : 30
                      },
                      "partnerSplits" : [
                        {
                          "partner" : "KECK",
                          "percent" : 100
                        }
                      ]
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

}
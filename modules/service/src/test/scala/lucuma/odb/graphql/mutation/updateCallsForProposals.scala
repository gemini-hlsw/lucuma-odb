// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.CallForProposals
import lucuma.core.util.DateInterval

import java.time.LocalDate
import java.time.Month

class updateCallsForProposals extends OdbSuite {

  val pi         = TestUsers.Standard.pi(nextId, nextId)
  val service    = TestUsers.service(nextId)
  val staff      = TestUsers.Standard.staff(nextId, nextId)
  val validUsers = List(pi, service, staff)

  val createCall: IO[CallForProposals.Id] =
    query(
      staff,
      s"""
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:      "2025A"
                activeStart:   "2025-02-01"
                activeEnd:     "2025-07-31"
                submissionDeadlineDefault: "2025-07-31T10:00:01Z"
                partners:      [
                  {
                    geminiPartner: CA
                    submissionDeadlineOverride: "2025-07-31T10:00:00Z"
                  },
                  {
                    geminiPartner: US
                  }
                ]
                gemini: {
                  type:          REGULAR_SEMESTER
                  coordinateLimits: {
                    north: {
                      raStart: { hms: "12:00:00" }
                      raEnd: { hms: "18:00:00" }
                      decStart: { dms: "45:00:00" }
                      decEnd: { dms: "-45:00:00" }
                    }
                    south: {
                      raStart: { hms: "12:00:01" }
                      raEnd: { hms: "18:00:01" }
                      decStart: { dms: "45:00:01" }
                      decEnd: { dms: "-45:00:01" }
                    }
                  }
                  instruments:   [GMOS_NORTH]
                }
              }
            }
          ) {
            callForProposals {
              id
            }
          }
        }
      """
    ).flatMap {
      _.hcursor
       .downFields("createCallForProposals", "callForProposals", "id")
       .as[CallForProposals.Id]
       .leftMap(f => new RuntimeException(f.message))
       .liftTo[IO]
    }

  test("type") {
    // changing the type should work, but it doesn't go back and silently set
    // the proprietaryMonths to match.
    createCall.flatMap: id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                gemini: {
                  type: DEMO_SCIENCE
                }
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                gemini {
                  type
                  proprietaryMonths
                }
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "gemini": {
                    "type": "DEMO_SCIENCE",
                    "proprietaryMonths": 12
                  }
                }
              ]
            }
          }
        """.asRight
      )
  }

  test("semester") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                semester: "2024B"
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                semester
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "semester": "2024B"
                }
              ]
            }
          }
        """.asRight
      )
    }
  }

  test("RA limits") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                gemini: {
                  coordinateLimits: {
                    north: {
                      raStart: { hms: "00:00:00" }
                      raEnd:   { hms: "06:00:00" }
                    }
                  }
                }
              }
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                gemini {
                  coordinateLimits {
                    north {
                      raStart { hms }
                      raEnd { hms }
                      decStart { dms }
                      decEnd { dms }
                    }
                    south {
                      raStart { hms }
                      raEnd { hms }
                      decStart { dms }
                      decEnd { dms }
                    }
                  }
                }
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "gemini": {
                    "coordinateLimits": {
                      "north": {
                        "raStart": { "hms": "00:00:00.000000" },
                        "raEnd": { "hms": "06:00:00.000000" },
                        "decStart": { "dms": "+45:00:00.000000" },
                        "decEnd": { "dms": "-45:00:00.000000" }
                      },
                      "south": {
                        "raStart": { "hms": "12:00:01.000000" },
                        "raEnd": { "hms": "18:00:01.000000" },
                        "decStart": { "dms": "+45:00:01.000000" },
                        "decEnd": { "dms": "-45:00:01.000000" }
                      }
                    }
                  }
                }
              ]
            }
          }
        """.asRight
      )
    }

  }

  test("Dec limits") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                gemini: {
                  coordinateLimits: {
                    south: {
                      decStart: { dms: "+10:00:00" }
                      decEnd:   { dms: "-50:00:00" }
                    }
                  }
                }
              }
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                gemini {
                  coordinateLimits {
                    north {
                      raStart { hms }
                      raEnd { hms }
                      decStart { dms }
                      decEnd { dms }
                    }
                    south {
                      raStart { hms }
                      raEnd { hms }
                      decStart { dms }
                      decEnd { dms }
                    }
                  }
                }
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "gemini": {
                    "coordinateLimits": {
                      "north": {
                        "raStart": { "hms": "12:00:00.000000" },
                        "raEnd": { "hms": "18:00:00.000000" },
                        "decStart": { "dms": "+45:00:00.000000" },
                        "decEnd": { "dms": "-45:00:00.000000" }
                      },
                      "south": {
                        "raStart": { "hms": "12:00:01.000000" },
                        "raEnd": { "hms": "18:00:01.000000" },
                        "decStart": { "dms": "+10:00:00.000000" },
                        "decEnd": { "dms": "-50:00:00.000000" }
                      }
                    }
                  }
                }
              ]
            }
          }
        """.asRight
      )
    }

  }

  test("active - start and end") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                activeStart: "2024-12-31"
                activeEnd:   "2026-01-01"
              },
              WHERE: {
                id: { EQ: "$id" }
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
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "active": {
                    "start": "2024-12-31",
                    "end":   "2026-01-01"
                  }
                }
              ]
            }
          }
        """.asRight

      )
    }
  }

  test("active - start only") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                activeStart: "2024-12-31"
              },
              WHERE: {
                id: { EQ: "$id" }
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
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "active": {
                    "start": "2024-12-31",
                    "end":   "2025-07-31"
                  }
                }
              ]
            }
          }
        """.asRight

      )
    }
  }

  test("active - end only") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                activeEnd: "2025-06-01"
              },
              WHERE: {
                id: { EQ: "$id" }
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
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "active": {
                    "start": "2025-02-01",
                    "end":   "2025-06-01"
                  }
                }
              ]
            }
          }
        """.asRight

      )
    }
  }

  test("active - end before start, both specified") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                activeStart: "2025-12-31"
                activeEnd:   "2025-01-01"
              },
              WHERE: {
                id: { EQ: "$id" }
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
        """,
        List("Argument 'input.SET' is invalid: 'activeStart' must come before 'activeEnd'").asLeft
      )
    }
  }

  test("active - end before start, start moved") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                activeStart: "2026-01-01"
              },
              WHERE: {
                id: { EQ: "$id" }
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
        """,
        List("Requested update to the active period is invalid: activeStart must come before activeEnd").asLeft
      )
    }
  }

  test("active - end before start, end moved") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                activeEnd: "2020-01-01"
              },
              WHERE: {
                id: { EQ: "$id" }
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
        """,
        List("Requested update to the active period is invalid: activeStart must come before activeEnd").asLeft
      )
    }
  }

  test("instruments") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                gemini: {
                  instruments: [GMOS_NORTH, GMOS_SOUTH]
                }
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                gemini {
                  instruments
                }
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "gemini": {
                    "instruments": [
                      "GMOS_NORTH",
                      "GMOS_SOUTH"
                    ]
                  }
                }
              ]
            }
          }
        """.asRight
      )
    }
  }

  test("instruments - delete") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                gemini: {
                  instruments: null
                }
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                gemini {
                  instruments
                }
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "gemini": {
                    "instruments": []
                  }
                }
              ]
            }
          }
        """.asRight
      )
    }
  }

  test("instruments - duplicate") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                gemini: {
                  instruments: [GMOS_SOUTH, GMOS_SOUTH]
                }
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                gemini {
                  instruments
                }
              }
            }
          }
        """,
        List("Argument 'input.SET.gemini' is invalid: duplicate 'instruments' specified: GMOS_SOUTH").asLeft
      )
    }
  }

  test("partners") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                partners: [
                  {
                    geminiPartner: BR
                    submissionDeadlineOverride: "2025-08-15T04:00:00Z"
                  },
                  {
                    geminiPartner: AR
                  }
                ]
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                partners {
                  geminiPartner
                  submissionDeadline
                }
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "partners": [
                    {
                      "geminiPartner": "AR",
                      "submissionDeadline": "2025-07-31T10:00:01Z"
                    },
                    {
                      "geminiPartner": "BR",
                      "submissionDeadline": "2025-08-15T04:00:00Z"
                    }
                  ]
                }
              ]
            }
          }
        """.asRight
      )
    }
  }

  test("partners - delete") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                partners: null
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                partners { geminiPartner }
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "partners": []
                }
              ]
            }
          }
        """.asRight
      )
    }
  }

  test("partners - duplicate") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                partners: [
                  {
                    geminiPartner: BR
                    submissionDeadlineOverride: "2025-08-15T04:00:00Z"
                  },
                  {
                    geminiPartner: BR
                    submissionDeadlineOverride: "2025-08-15T04:00:00Z"
                  }
                ]
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                gemini {
                  instruments
                }
              }
            }
          }
        """,
        List("Argument 'input.SET' is invalid: duplicate 'partners' specified: BR").asLeft
      )
    }
  }

  test("partners - remove default deadline") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                submissionDeadlineDefault: null
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                partners {
                  geminiPartner
                  submissionDeadline
                }
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "partners": [
                    {
                      "geminiPartner": "CA",
                      "submissionDeadline": "2025-07-31T10:00:00Z"
                    },
                    {
                      "geminiPartner": "US",
                      "submissionDeadline": null
                    }
                  ]
                }
              ]
            }
          }
        """.asRight
      )
    }
  }

  test("existence") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                existence: DELETED
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                existence
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "existence": "DELETED"
                }
              ]
            }
          }
        """.asRight
      )
    }
  }

  test("cannot delete an in-use Cfp") {
    for {
      cid <- createCall
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                existence: DELETED
              },
              WHERE: {
                id: { EQ: "$cid" }
              }
            }) {
              callsForProposals {
                existence
              }
            }
          }
        """,
        List(s"""Cannot delete this Call for Proposals, or change its type or semester, because dependent proposals reference it: ["$pid"].""").asLeft
      )
    } yield ()
  }

  test("cannot change the semester in an in-use Cfp") {
    for {
      cid <- createCall
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                semester: "2024B"
              },
              WHERE: {
                id: { EQ: "$cid" }
              }
            }) {
              callsForProposals {
                semester
              }
            }
          }
        """,
        List(s"""Cannot delete this Call for Proposals, or change its type or semester, because dependent proposals reference it: ["$pid"].""").asLeft
      )
    } yield ()
  }

  test("cannot change the type in an in-use Cfp") {
    for {
      cid <- createCall
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                gemini: {
                  type: DEMO_SCIENCE
                }
              },
              WHERE: {
                id: { EQ: "$cid" }
              }
            }) {
              callsForProposals {
                gemini {
                  type
                }
              }
            }
          }
        """,
        List(s"""Cannot delete this Call for Proposals, or change its type or semester, because dependent proposals reference it: ["$pid"].""").asLeft
      )
    } yield ()
  }

  test("can change the active period in an in-use Cfp") {
    for {
      cid <- createCall
      pid <- createProgramAs(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- expect(
        staff,
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
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "active": {
                    "start": "2024-12-31",
                    "end":   "2026-01-01"
                  }
                }
              ]
            }
          }
        """.asRight
      )
      _ <- assertIOBoolean(getActivePeriod(pi, pid).map(_ === DateInterval.between(LocalDate.of(2024, Month.DECEMBER, 31), LocalDate.of(2026, Month.JANUARY, 1))))
    } yield ()
  }

  test("proprietaryMonths") {
    createCall.flatMap: id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                gemini: {
                  proprietaryMonths: 36
                }
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                gemini {
                  proprietaryMonths
                }
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "gemini": {
                    "proprietaryMonths": 36
                  }
                }
              ]
            }
          }
        """.asRight
      )
  }

  test("title"):
    createCall.flatMap: id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                title: "Foo"
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                title
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "title": "Foo"
                }
              ]
            }
          }
        """.asRight
      )

  test("unset title"):
    createCall.flatMap: id =>
      query(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: { title: "Foo" },
              WHERE: { id: { EQ: "$id" } }
            }) {
              callsForProposals { title }
            }
          }
        """
      ) >> expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                title: null
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                title
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "title": "2025A Regular Semester"
                }
              ]
            }
          }
        """.asRight
      )
}

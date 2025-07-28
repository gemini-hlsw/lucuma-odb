// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CallForProposalsType
import lucuma.core.model.CallForProposals
import lucuma.core.syntax.string.*

class callForProposals extends OdbSuite {

  val pi    = TestUsers.Standard.pi(1, 30)
  val staff = TestUsers.Standard.staff(3, 103)
  override val validUsers = List(pi, staff)

  private val createCall: IO[CallForProposals.Id] =
    query(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:        REGULAR_SEMESTER
                semester:    "2025A"
                activeStart: "2025-02-01"
                activeEnd:   "2025-07-31"
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

  test("null result") {
    expect(
      user  = pi,
      query = s"""
        query {
          callForProposals(callForProposalsId: "c-123") {
            id
          }
        }
      """,
      expected = json"""
        {
           "callForProposals": null
        }
      """.asRight
    )
  }

  test("not null result") {
    createCall.flatMap { cid =>
      expect(
        user  = pi,
        query = s"""
          query {
            callForProposals(callForProposalsId: "$cid") {
              id
            }
          }
        """,
        expected = json"""
          {
            "callForProposals": {
              "id": ${cid.asJson}
            }
          }
        """.asRight
      )
    }
  }

  private def getTitle(
    cfpType:       CallForProposalsType,
    titleOverride: Option[String] = None
  ): IO[String] =
    query(
      user = staff,
      query = s"""
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:        ${cfpType.tag.toScreamingSnakeCase}
                semester:    "2025A"
                ${titleOverride.fold("")(title => s"titleOverride: \"$title\"")}
                activeStart: "2025-02-01"
                activeEnd:   "2025-07-31"
              }
            }
          ) {
            callForProposals {
              title
            }
          }
        }
      """
    ).flatMap {
      _.hcursor
       .downFields("createCallForProposals", "callForProposals", "title")
       .as[String]
       .leftMap(f => new RuntimeException(f.message))
       .liftTo[IO]
    }

  test("demo science") {
    assertIO(getTitle(CallForProposalsType.DemoScience), "2025A Demo Science")
  }

  test("title override"):
    assertIO(getTitle(CallForProposalsType.DemoScience, "Foo".some), "Foo")

  test("director's time") {
    assertIO(getTitle(CallForProposalsType.DirectorsTime), "2025A Director's Time")
  }

  test("large program") {
    assertIO(getTitle(CallForProposalsType.LargeProgram), "2025A Large Program")
  }

  test("poor weather") {
    assertIO(getTitle(CallForProposalsType.PoorWeather), "2025A Poor Weather")
  }

  test("regular semester") {
    assertIO(getTitle(CallForProposalsType.RegularSemester), "2025A Regular Semester")
  }

  test("fast turnaround") {
    val title = query(
      user = staff,
      query = s"""
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:        FAST_TURNAROUND
                semester:    "2025A"
                activeStart: "2025-08-01"
                activeEnd:   "2025-08-31"
              }
            }
          ) {
            callForProposals {
              title
            }
          }
        }
      """
    ).flatMap {
      _.hcursor
       .downFields("createCallForProposals", "callForProposals", "title")
       .as[String]
       .leftMap(f => new RuntimeException(f.message))
       .liftTo[IO]
    }

    assertIO(title, "2025 June Fast Turnaround")
  }

  test("system verification - no instruments") {
    assertIO(getTitle(CallForProposalsType.SystemVerification), "2025A System Verification")
  }

  test("system verification - one instrument") {
    val title = query(
      user = staff,
      query = s"""
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:        SYSTEM_VERIFICATION
                semester:    "2025A"
                activeStart: "2025-08-01"
                activeEnd:   "2025-08-31"
                instruments: [GMOS_NORTH]
              }
            }
          ) {
            callForProposals {
              title
            }
          }
        }
      """
    ).flatMap {
      _.hcursor
       .downFields("createCallForProposals", "callForProposals", "title")
       .as[String]
       .leftMap(f => new RuntimeException(f.message))
       .liftTo[IO]
    }

    assertIO(title, "2025A GMOS North System Verification")

  }

  test("system verification - two instruments") {
    val title = query(
      user = staff,
      query = s"""
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:        SYSTEM_VERIFICATION
                semester:    "2025A"
                activeStart: "2025-08-01"
                activeEnd:   "2025-08-31"
                instruments: [GMOS_SOUTH, GMOS_NORTH]
              }
            }
          ) {
            callForProposals {
              title
            }
          }
        }
      """
    ).flatMap {
      _.hcursor
       .downFields("createCallForProposals", "callForProposals", "title")
       .as[String]
       .leftMap(f => new RuntimeException(f.message))
       .liftTo[IO]
    }

    assertIO(title, "2025A GMOS North, GMOS South System Verification")

  }
}

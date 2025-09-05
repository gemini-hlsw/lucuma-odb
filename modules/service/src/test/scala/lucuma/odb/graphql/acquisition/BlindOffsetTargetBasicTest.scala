// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package acquisition

import io.circe.syntax.*
import lucuma.core.model.Target
import lucuma.core.model.User

class BlindOffsetTargetBasicTest extends OdbSuite {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  val pi2: User = TestUsers.Standard.pi(nextId, nextId)
  override lazy val validUsers: List[User] = List(pi, pi2)

  test("useBlindOffset field should be available") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      response <- query(
        user = pi,
        query = s"""
          query {
            observation(observationId: "$oid") {
              useBlindOffset
              blindOffsetTarget {
                id
                name
              }
            }
          }
        """
      )
    } yield {
      val useBlindOffset = response.hcursor.downField("data").downField("observation").downField("useBlindOffset").as[Boolean]
      assertEquals(useBlindOffset, Right(false))
    }
  }

  test("useBlindOffset should default to false for new observations") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      result <- query(
        user = pi,
        query = s"""
          query {
            observation(observationId: "$oid") {
              useBlindOffset
            }
          }
        """
      )
    } yield {
      val useBlindOffset = result.hcursor.downField("data").downField("observation").downField("useBlindOffset").as[Boolean]
      assertEquals(useBlindOffset, Right(false))
    }
  }

  test("blindOffsetTarget field should be nullable") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      response <- query(
        user = pi,
        query = s"""
          query {
            observation(observationId: "$oid") {
              blindOffsetTarget {
                id
              }
            }
          }
        """
      )
    } yield {
      val blindOffsetTarget = response.hcursor.downField("data").downField("observation").downField("blindOffsetTarget")
      assert(blindOffsetTarget.focus.exists(_.isNull))
    }
  }

  test("create observation with useBlindOffset set to true") {
    for {
      pid <- createProgramAs(pi)
      result <- query(
        user = pi,
        query = s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                useBlindOffset: true
              }
            }) {
              observation {
                id
                useBlindOffset
              }
            }
          }
        """
      )
    } yield {
      val useBlindOffset = result.hcursor
        .downField("data")
        .downField("createObservation")
        .downField("observation")
        .downField("useBlindOffset")
        .as[Boolean]
      assertEquals(useBlindOffset, Right(true))
    }
  }

  test("create observation with blind offset target") {
    for {
      pid <- createProgramAs(pi)
      result <- query(
        user = pi,
        query = s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                blindOffsetTarget: {
                  name: "Blind Offset Star"
                  sidereal: {
                    ra: { degrees: "12.345" }
                    dec: { degrees: "45.678" }
                    epoch: "J2000.000"
                  }
                }
              }
            }) {
              observation {
                id
                blindOffsetTarget {
                  id
                  name
                }
              }
            }
          }
        """
      )
    } yield {
      val blindOffsetTarget = result.hcursor
        .downField("data")
        .downField("createObservation")
        .downField("observation")
        .downField("blindOffsetTarget")
      val name = blindOffsetTarget.downField("name").as[String]
      assertEquals(name, Right("Blind Offset Star"))
      assert(blindOffsetTarget.downField("id").as[Target.Id].isRight)
    }
  }

  test("update observation to add blind offset target") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      result <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              observationIds: [${oid.asJson}]
              SET: {
                blindOffsetTarget: {
                  name: "New Blind Offset Target"
                  sidereal: {
                    ra: { degrees: "98.765" }
                    dec: { degrees: "-12.345" }
                    epoch: "J2000.000"
                  }
                }
              }
            }) {
              observations {
                id
                blindOffsetTarget {
                  id
                  name
                }
              }
            }
          }
        """
      )
    } yield {
      val blindOffsetTarget = result.hcursor
        .downField("data")
        .downField("updateObservations")
        .downField("observations")
        .downN(0)
        .downField("blindOffsetTarget")
      val name = blindOffsetTarget.downField("name").as[String]
      assertEquals(name, Right("New Blind Offset Target"))
      assert(blindOffsetTarget.downField("id").as[Target.Id].isRight)
    }
  }

  test("update observation to modify existing blind offset target") {
    for {
      pid <- createProgramAs(pi)
      // First create observation with blind offset target
      oid <- createObservationAs(pi, pid)
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              observationIds: [${oid.asJson}]
              SET: {
                blindOffsetTarget: {
                  name: "Original Target"
                  sidereal: {
                    ra: { degrees: "12.345" }
                    dec: { degrees: "45.678" }
                    epoch: "J2000.000"
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """
      )
      // Then modify the blind offset target
      result <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              observationIds: [${oid.asJson}]
              SET: {
                blindOffsetTarget: {
                  name: "Modified Target"
                  sidereal: {
                    ra: { degrees: "99.999" }
                    dec: { degrees: "-88.888" }
                    epoch: "J2000.000"
                  }
                }
              }
            }) {
              observations {
                id
                blindOffsetTarget {
                  id
                  name
                }
              }
            }
          }
        """
      )
    } yield {
      val blindOffsetTarget = result.hcursor
        .downField("data")
        .downField("updateObservations")
        .downField("observations")
        .downN(0)
        .downField("blindOffsetTarget")
      val name = blindOffsetTarget.downField("name").as[String]
      assertEquals(name, Right("Modified Target"))
    }
  }

  test("update observation to remove blind offset target") {
    for {
      pid <- createProgramAs(pi)
      // First create observation with blind offset target
      oid <- createObservationAs(pi, pid)
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              observationIds: [${oid.asJson}]
              SET: {
                blindOffsetTarget: {
                  name: "Target to Remove"
                  sidereal: {
                    ra: { degrees: "12.345" }
                    dec: { degrees: "45.678" }
                    epoch: "J2000.000"
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """
      )
      // Then remove the blind offset target by setting to null
      result <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              observationIds: [${oid.asJson}]
              SET: {
                blindOffsetTarget: null
              }
            }) {
              observations {
                id
                blindOffsetTarget {
                  id
                  name
                }
              }
            }
          }
        """
      )
    } yield {
      val blindOffsetTarget = result.hcursor
        .downField("data")
        .downField("updateObservations")
        .downField("observations")
        .downN(0)
        .downField("blindOffsetTarget")
      assert(blindOffsetTarget.focus.exists(_.isNull))
    }
  }

  test("update observation to set useBlindOffset") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      result <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              observationIds: [${oid.asJson}]
              SET: {
                useBlindOffset: true
              }
            }) {
              observations {
                id
                useBlindOffset
              }
            }
          }
        """
      )
    } yield {
      val useBlindOffset = result.hcursor
        .downField("data")
        .downField("updateObservations")
        .downField("observations")
        .downN(0)
        .downField("useBlindOffset")
        .as[Boolean]
      assertEquals(useBlindOffset, Right(true))
    }
  }

  test("update multiple observations with same blind offset target should succeed") {
    for {
      pid <- createProgramAs(pi)
      oid1 <- createObservationAs(pi, pid)
      oid2 <- createObservationAs(pi, pid)
      result <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              observationIds: [${oid1.asJson}, ${oid2.asJson}]
              SET: {
                blindOffsetTarget: {
                  name: "Shared Blind Offset Target"
                  sidereal: {
                    ra: { degrees: "12.345" }
                    dec: { degrees: "45.678" }
                    epoch: "J2000.000"
                  }
                }
              }
            }) {
              observations {
                id
                blindOffsetTarget {
                  id
                }
              }
            }
          }
        """
      )
    } yield {
      // Should succeed because each observation gets its own copy of the target
      val observations = result.hcursor
        .downField("data")
        .downField("updateObservations")
        .downField("observations")
      val target1Id = observations.downN(0).downField("blindOffsetTarget").downField("id").as[Target.Id]
      val target2Id = observations.downN(1).downField("blindOffsetTarget").downField("id").as[Target.Id]
      assert(target1Id.isRight)
      assert(target2Id.isRight)
      assert(target1Id != target2Id) // Each should get a different target ID
    }
  }

}
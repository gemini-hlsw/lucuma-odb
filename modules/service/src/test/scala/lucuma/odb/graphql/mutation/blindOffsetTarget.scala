// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.syntax.*
import lucuma.core.model.User

class blindOffsetTarget extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)
  val pi2: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi, pi2)

  private def blindOffsetInput(name: String, ra: String, dec: String): String =
    s"""
      name: "$name"
      sidereal: {
        ra: { degrees: "$ra" }
        dec: { degrees: "$dec" }
        epoch: "J2000.000"
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
    """


  private val targetEnvironmentFields =
    """
      targetEnvironment {
      blindOffsetTarget {
        id
        name
      }
    }
    """

  private val observationsFields =
    s"""
      observations {
        id
        $targetEnvironmentFields
      }
    """

  private def blindOffsetTargetInput(createInput: String): String =
    s"""
      blindOffsetTarget: {
        $createInput
      }
    """

  test("create observation with blind offset target"):
    for {
      pid <- createProgramAs(pi)
      result <- query(
        user = pi,
        query = s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Blind Offset Star", "12.345", "45.678"))}
                }
              }
            }) {
              observation {
                id
                $targetEnvironmentFields
              }
            }
          }
        """
      )
    } yield {
      val blindOffsetTarget = result.hcursor
        .downField("createObservation")
        .downField("observation")
        .downField("targetEnvironment")
        .downField("blindOffsetTarget")
      val name = blindOffsetTarget.downField("name").as[String]
      assertEquals(name, Right("Blind Offset Star"))
    }

  test("update observation to add blind offset target"):
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      result <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("New Blind Offset Target", "98.765", "-12.345"))}
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              $observationsFields
            }
          }
        """
      )
    } yield {
      val blindOffsetTarget = result.hcursor
        .downField("updateObservations")
        .downField("observations")
        .downN(0)
        .downField("targetEnvironment")
        .downField("blindOffsetTarget")
      val name = blindOffsetTarget.downField("name").as[String]
      assertEquals(name, Right("New Blind Offset Target"))
    }

  test("update observation to replace blind offset target"):
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Original Target", "12.345", "45.678"))}
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              observations { id }
            }
          }
        """
      )
      result <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Replacement Target", "99.999", "-88.888"))}
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              $observationsFields
            }
          }
        """
      )
    } yield {
      val blindOffsetTarget = result.hcursor
        .downField("updateObservations")
        .downField("observations")
        .downN(0)
        .downField("targetEnvironment")
        .downField("blindOffsetTarget")
      val name = blindOffsetTarget.downField("name").as[String]
      assertEquals(name, Right("Replacement Target"))
    }

  test("update observation to remove blind offset target"):
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      _ <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  ${blindOffsetTargetInput(blindOffsetInput("Target to Remove", "12.345", "45.678"))}
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              observations { id }
            }
          }
        """
      )
      result <- query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  blindOffsetTarget: null
                }
              }
              WHERE: {
                id: { EQ: ${oid.asJson} }
              }
            }) {
              $observationsFields
            }
          }
        """
      )
    } yield {
      val blindOffsetTarget = result.hcursor
        .downField("updateObservations")
        .downField("observations")
        .downN(0)
        .downField("targetEnvironment")
        .downField("blindOffsetTarget")
      assert(blindOffsetTarget.focus.exists(_.isNull))
    }

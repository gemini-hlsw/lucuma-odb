// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.model.CallForProposals
import lucuma.core.model.Observation
import lucuma.core.model.Program

class updateObservations_Exchange extends OdbSuite with DatabaseOperations:

  val pi    = TestUsers.Standard.pi(1, 101)
  val staff = TestUsers.Standard.staff(3, 103)

  override val validUsers = List(pi, staff)

  // Create a program whose proposal is attached to the given exchange call, so
  // the program's observatory is keck/subaru.
  private def createProgramWithProposal(cid: CallForProposals.Id, set: String): IO[Program.Id] =
    for
      pid <- createProgramAs(pi, "exchange program")
      _   <- query(pi, s"""
               mutation {
                 createProposal(input: {
                   programId: "$pid"
                   SET: { callId: "$cid", $set }
                 }) { proposal { call { id } } }
               }
             """)
    yield pid

  private def subaruProgram: IO[Program.Id] =
    createSubaruCallForProposalsAs(staff).flatMap: cid =>
      createProgramWithProposal(cid, """subaru: { type: NORMAL, partnerSplits: [{ partner: US, percent: 100 }] }""")

  private def keckProgram: IO[Program.Id] =
    createKeckCallForProposalsAs(staff).flatMap: cid =>
      createProgramWithProposal(cid, """keck: { partnerSplits: [{ partner: US, percent: 100 }] }""")

  private def createExchangeObservationAs(pid: Program.Id, exchange: String): IO[Observation.Id] =
    for
      tid <- createTargetAs(pi, pid)
      oid <- query(pi, s"""
               mutation {
                 createObservation(input: {
                   programId: "$pid"
                   SET: {
                     targetEnvironment: { asterism: [ "$tid" ] }
                     observingMode: { exchange: { $exchange } }
                   }
                 }) { observation { id } }
               }
             """).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])
    yield oid

  private def updateExchangeQuery(oid: Observation.Id, exchange: String): String =
    s"""
      mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: { observingMode: { exchange: { $exchange } } }
        }) {
          observations {
            observingMode {
              exchange {
                mode
                keckInstrument
                subaruInstrument
                totalRequestTime { hours }
              }
            }
          }
        }
      }
    """

  test("update: change totalRequestTime only"):
    for
      pid <- subaruProgram
      oid <- createExchangeObservationAs(pid, "subaruInstrument: FOCAS, totalRequestTime: { hours: 3 }")
      _   <- expect(
        user  = pi,
        query = updateExchangeQuery(oid, "totalRequestTime: { hours: 5 }"),
        expected = json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "observingMode": {
                    "exchange": {
                      "mode": "EXCHANGE_SUBARU",
                      "keckInstrument": null,
                      "subaruInstrument": "FOCAS",
                      "totalRequestTime": { "hours": 5.000000 }
                    }
                  }
                }
              ]
            }
          }
        """.asRight
      )
    yield ()

  test("update: change instrument within the same observatory"):
    for
      pid <- subaruProgram
      oid <- createExchangeObservationAs(pid, "subaruInstrument: FOCAS, totalRequestTime: { hours: 3 }")
      _   <- expect(
        user  = pi,
        query = updateExchangeQuery(oid, "subaruInstrument: HDS"),
        expected = json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "observingMode": {
                    "exchange": {
                      "mode": "EXCHANGE_SUBARU",
                      "keckInstrument": null,
                      "subaruInstrument": "HDS",
                      "totalRequestTime": { "hours": 3.000000 }
                    }
                  }
                }
              ]
            }
          }
        """.asRight
      )
    yield ()

  test("update: switch observatory (Subaru -> Keck)"):
    for
      pid <- subaruProgram
      oid <- createExchangeObservationAs(pid, "subaruInstrument: FOCAS, totalRequestTime: { hours: 3 }")
      _   <- expect(
        user  = pi,
        query = updateExchangeQuery(oid, "keckInstrument: HIRES, totalRequestTime: { hours: 2 }"),
        expected = json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "observingMode": {
                    "exchange": {
                      "mode": "EXCHANGE_KECK",
                      "keckInstrument": "HIRES",
                      "subaruInstrument": null,
                      "totalRequestTime": { "hours": 2.000000 }
                    }
                  }
                }
              ]
            }
          }
        """.asRight
      )
    yield ()

  test("clone: cloned exchange observation has the same config"):
    for
      pid <- keckProgram
      oid <- createExchangeObservationAs(pid, "keckInstrument: HIRES, totalRequestTime: { hours: 4 }")
      _   <- expect(
        user  = pi,
        query = s"""
          mutation {
            cloneObservation(input: { observationId: "$oid" }) {
              newObservation {
                observingMode {
                  exchange {
                    mode
                    keckInstrument
                    subaruInstrument
                    totalRequestTime { hours }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "cloneObservation": {
              "newObservation": {
                "observingMode": {
                  "exchange": {
                    "mode": "EXCHANGE_KECK",
                    "keckInstrument": "HIRES",
                    "subaruInstrument": null,
                    "totalRequestTime": { "hours": 4.000000 }
                  }
                }
              }
            }
          }
        """.asRight
      )
    yield ()

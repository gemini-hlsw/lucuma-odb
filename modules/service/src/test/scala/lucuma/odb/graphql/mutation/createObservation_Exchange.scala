// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.model.CallForProposals
import lucuma.core.model.Program

class createObservation_Exchange extends OdbSuite with DatabaseOperations:

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
      createProgramWithProposal(cid, """subaru: { partnerSplits: [{ partner: US, percent: 100 }] }""")

  private def keckProgram: IO[Program.Id] =
    createKeckCallForProposalsAs(staff).flatMap: cid =>
      createProgramWithProposal(cid, """keck: { partnerSplits: [{ partner: US, percent: 100 }] }""")

  private def createExchangeQuery(pid: String, tid: String, exchange: String): String =
    s"""
      mutation {
        createObservation(input: {
          programId: "$pid"
          SET: {
            targetEnvironment: { asterism: [ "$tid" ] }
            observingMode: { exchange: { $exchange } }
          }
        }) {
          observation {
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

  test("Subaru exchange observation succeeds on a Subaru proposal"):
    for
      pid <- subaruProgram
      tid <- createTargetAs(pi, pid)
      _   <- expect(
        user  = pi,
        query = createExchangeQuery(pid.toString, tid.toString,
          "subaruInstrument: FOCAS, totalRequestTime: { hours: 3 }"),
        expected = json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "exchange": {
                    "mode": "EXCHANGE_SUBARU",
                    "keckInstrument": null,
                    "subaruInstrument": "FOCAS",
                    "totalRequestTime": { "hours": 3.000000 }
                  }
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("Keck exchange observation succeeds on a Keck proposal"):
    for
      pid <- keckProgram
      tid <- createTargetAs(pi, pid)
      _   <- expect(
        user  = pi,
        query = createExchangeQuery(pid.toString, tid.toString,
          "keckInstrument: HIRES, totalRequestTime: { hours: 1 }"),
        expected = json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "exchange": {
                    "mode": "EXCHANGE_KECK",
                    "keckInstrument": "HIRES",
                    "subaruInstrument": null,
                    "totalRequestTime": { "hours": 1.000000 }
                  }
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  // Observatory consistency (exchange mode vs. the proposal's observatory) is a
  // workflow validation, not a create-time gate, mirroring how Gemini validates
  // instrument-vs-CfP.  So creation succeeds here regardless; the dedicated
  // workflow-validation coverage is added in a later iteration.
  test("Keck exchange observation can be created even on a Subaru proposal (validated at workflow)"):
    for
      pid <- subaruProgram
      tid <- createTargetAs(pi, pid)
      _   <- expect(
        user  = pi,
        query = createExchangeQuery(pid.toString, tid.toString,
          "keckInstrument: HIRES, totalRequestTime: { hours: 1 }"),
        expected = json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "exchange": {
                    "mode": "EXCHANGE_KECK",
                    "keckInstrument": "HIRES",
                    "subaruInstrument": null,
                    "totalRequestTime": { "hours": 1.000000 }
                  }
                }
              }
            }
          }
        """.asRight
      )
    yield ()

  test("Exchange observation can be created on a program with no exchange proposal (validated at workflow)"):
    for
      pid <- createProgramAs(pi, "gemini program")
      tid <- createTargetAs(pi, pid)
      _   <- expect(
        user  = pi,
        query = createExchangeQuery(pid.toString, tid.toString,
          "subaruInstrument: FOCAS, totalRequestTime: { hours: 1 }"),
        expected = json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "exchange": {
                    "mode": "EXCHANGE_SUBARU",
                    "keckInstrument": null,
                    "subaruInstrument": "FOCAS",
                    "totalRequestTime": { "hours": 1.000000 }
                  }
                }
              }
            }
          }
        """.asRight
      )
    yield ()
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import lucuma.core.enums.KeckInstrument
import lucuma.core.enums.Observatory
import lucuma.core.model.CallForProposals
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.service.ObservationWorkflowService.Messages

class observation_workflow_exchange extends OdbSuite with DatabaseOperations:

  val pi: User          = TestUsers.Standard.pi(1, 30)
  val serviceUser       = TestUsers.service(3)
  val staff: User       = TestUsers.Standard.staff(4, 33)

  val validUsers = List(pi, serviceUser, staff)

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

  // Returns the validationErrors reported by the observation's workflow.
  private def validationErrors(oid: Observation.Id): IO[List[ObservationValidation]] =
    query(pi, s"""
      query {
        observations(WHERE: { id: { IN: ${List(oid).asJson} } }) {
          matches {
            workflow {
              value {
                validationErrors {
                  code
                  messages
                }
              }
            }
          }
        }
      }
    """).map: js =>
      js.hcursor
        .downFields("observations", "matches")
        .downN(0)
        .downFields("workflow", "value", "validationErrors")
        .require[List[ObservationValidation]]

  test("exchange observatory mismatch is reported as a workflow validation"):
    for
      cid <- createSubaruCallForProposalsAs(staff)
      pid <- createProgramWithProposal(cid, """subaru: { partnerSplits: [{ partner: US, percent: 100 }] }""")
      // A Keck exchange observation on a Subaru proposal.
      oid <- createExchangeObservationAs(pid, "keckInstrument: HIRES, totalRequestTime: { hours: 1 }")
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      ves <- validationErrors(oid)
      _   <- IO(assert(
               ves.contains_(ObservationValidation.callForProposals(Messages.exchangeObservatoryMismatch(Observatory.Keck, Observatory.Subaru))),
               s"Expected observatory-mismatch validation, got: $ves"
             ))
    yield ()

  test("exchange instrument not in the Call for Proposals is reported as a workflow validation"):
    for
      cid <- createKeckCallForProposalsAs(staff, instruments = List(KeckInstrument.Other))
      pid <- createProgramWithProposal(cid, """keck: { partnerSplits: [{ partner: US, percent: 100 }] }""")
      // HIRES is not in the call's allowed instrument list (only OTHER).
      oid <- createExchangeObservationAs(pid, "keckInstrument: HIRES, totalRequestTime: { hours: 1 }")
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      ves <- validationErrors(oid)
      _   <- IO(assert(
               ves.contains_(ObservationValidation.callForProposals(Messages.invalidExchangeInstrument(KeckInstrument.Hires.tag))),
               s"Expected invalid-instrument validation, got: $ves"
             ))
    yield ()

  test("a matching exchange observation has no call-for-proposals validation"):
    for
      cid <- createKeckCallForProposalsAs(staff, instruments = List(KeckInstrument.Hires))
      pid <- createProgramWithProposal(cid, """keck: { partnerSplits: [{ partner: US, percent: 100 }] }""")
      oid <- createExchangeObservationAs(pid, "keckInstrument: HIRES, totalRequestTime: { hours: 1 }")
      _   <- runObscalcUpdateAs(serviceUser, pid, oid)
      ves <- validationErrors(oid)
      _   <- IO(assert(
               !ves.exists(v => v.messages.exists(_.toLowerCase.contains("call for proposals"))),
               s"Expected no exchange call-for-proposals validation, got: $ves"
             ))
    yield ()

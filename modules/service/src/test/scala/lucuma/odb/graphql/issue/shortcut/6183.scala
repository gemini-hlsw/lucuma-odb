// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepConfig
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.json.all.transport.given
import lucuma.odb.graphql.mutation.UpdateConstraintSetOps
import io.circe.syntax.*
import io.circe.literal.*
import lucuma.odb.data.OdbError.InvalidObservation
import lucuma.core.model.User
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.core.syntax.string.*
import lucuma.core.util.Enumerated

class ShortCut_6183 
  extends ExecutionTestSupportForGmos
     with ObservingModeSetupOperations
     with UpdateConstraintSetOps:

  def queryObservationWorkflowState(oid: Observation.Id): IO[ObservationWorkflowState] =
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            workflow {
              value {
                state
              }
            }
          }
        }
        """
    ).map: json =>
      json.hcursor.downFields("observation", "workflow", "value", "state").require[ObservationWorkflowState]

  def createOngoingObservation(active: Boolean): IO[Observation.Id] =
    for
      p  <- createProgramAs(pi)
      t  <- createTargetWithProfileAs(pi, p)
      o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
      s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s0)
      s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
      _  <- addEndStepEvent(s1)
      s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science)
      _  <- addEndStepEvent(s2)
      _  <- runObscalcUpdate(p, o)
      _  <- setObservationWorkflowState(pi, o, ObservationWorkflowState.Inactive).unlessA(active)
      _  <- assertIO(queryObservationWorkflowState(o), ObservationWorkflowState.Ongoing).whenA(active)
      _  <- assertIO(queryObservationWorkflowState(o), ObservationWorkflowState.Ongoing).unlessA(active)
    yield o


  List(true, false).foreach { active =>

    val prefix = if active then "[Ongoing]" else "[Ongoing/Inactive]"

    def expectFailureAs(user: User, SET: String): IO[Unit] =
      createOngoingObservation(active).flatMap: oid =>
        expectOdbError(
          user = user,
          query = s"""
            mutation {
              updateObservations(input: {
                SET: $SET,
                WHERE: {
                  id: { EQ: ${oid.asJson} }
                }
              }) {
                observations {
                  id
                }
              }
            }
          """,
          expected = {
            case InvalidObservation(oid, Some(s)) if s.contains("ineligibile for this operation due to its workflow") => ()
          }
        )

    def expectSuccessAs(user: User, SET: String): IO[Unit] =
      createOngoingObservation(active).flatMap: oid =>
        expectSuccessOrOdbError(
          user = user,
          query = s"""
            mutation {
              updateObservations(input: {
                SET: $SET,
                WHERE: {
                  id: { EQ: ${oid.asJson} }
                }
              }) {
                observations {
                  id
                }
              }
            }
          """,
          expected = Right(
            json"""
              {
                "updateObservations" : {
                  "observations" : [
                    {
                      "id" : $oid
                    }
                  ]
                }
              }
            """
          )
        )

    test(s"$prefix Prevent editing of the contraints."):
      expectFailureAs(
        user = staff,
        SET = """{
          constraintSet: {
            skyBackground: DARKEST
          }
        }"""
      )

    test(s"$prefix Prevent editing of the observing mode."):
      expectFailureAs(
        user = staff,
        SET = """{
          observingMode: {
            gmosNorthImaging: {
              filters: [R_PRIME, G_PRIME]
            }
          }
        }"""
      )

    test(s"$prefix Prevent editing of the exposure mode."):
      expectFailureAs(
        user = staff,
        SET = """{
          scienceRequirements: {
            exposureTimeMode: {
              timeAndCount: {
                time: {
                  seconds: 12
                }
                count: 34
                at: {
                  nanometers: 56
                }
              }
            }
          }
        }"""
      )

    // test(s"$prefix Prevent editing of any targets associated with executed observations."):
    //   fail("not implemented")

    // test(s"$prefix Prevent deleting custom SEDs for a targets associated with executed observations"):
    //   fail("not implemented")

    // test(s"$prefix Prevent uploading a new custom SEDs for a targets associated with executed observations"):
    //   fail("not implemented")

    // test(s"$prefix Prevent adding/removing targets to/from the asterism."):
    //   fail("not implemented")

    test(s"$prefix Prevent editing of the Position Angle (PI)."):
      expectFailureAs(
        user = pi,
        SET = """{
          posAngleConstraint: {
            mode: AVERAGE_PARALLACTIC
          }
        }"""
      )

    test(s"$prefix Allow staff to set PA mode to Average Parallactic and Parallactic Override."):
      import PosAngleConstraintMode.*
      List(AverageParallactic, ParallacticOverride).traverse: mode =>
        expectSuccessAs(
          user = staff,
          SET = s"""{
            posAngleConstraint: {
              mode: ${mode.dbTag.toScreamingSnakeCase}
            }
          }"""
        )

    test(s"$prefix Prevent staff setting PA mode to anything other than Average Parallactic and Parallactic Override."):
      import PosAngleConstraintMode.*
      Enumerated[PosAngleConstraintMode].all.filterNot(Set(AverageParallactic, ParallacticOverride)).traverse: mode =>
        expectFailureAs(
          user = staff,
          SET = s"""{
            posAngleConstraint: {
              mode: ${mode.dbTag.toScreamingSnakeCase}
            }
          }"""
        )

    // test(s"$prefix Prevent staff changing explicit PA."):
    //   expectFailureAs(
    //     user = pi,
    //     SET = """{
    //       posAngleConstraint: {
    //         mode: AVERAGE_PARALLACTIC
    //       }
    //     }"""
    //   )

    // test(s"$prefix Allow editing of scheduling windows."):
    //   fail("not implemented")

    // test(s"$prefix Prevent adding/removing MosMask attachments."):
    //   fail("not implemented")

    // test(s"$prefix Allow adding/removing finder charts attachments."):
    //   fail("not implemented")

  }

  // test("Completed: Prevent editing of the Position Angle."):
  //   fail("not implemented")

  // test("Completed: Prevent editing of scheduling windows."):
  //   fail("not implemented")

  // test("Completed: Prevent adding/removing finder charts from the observation."):
  //   fail("not implemented")


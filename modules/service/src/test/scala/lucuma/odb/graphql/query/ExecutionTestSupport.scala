// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.Clock
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.syntax.string.*
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.service.Services
import lucuma.odb.service.UserService
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.implicits.*

import java.time.Instant

trait ExecutionTestSupport extends OdbSuite with ObservingModeSetupOperations with GenerationTestSupport {

  val pi: User          = TestUsers.Standard.pi(1, 30)
  val pi2: User         = TestUsers.Standard.pi(2, 32)
  val serviceUser       = TestUsers.service(3)
  val staff: User       = TestUsers.Standard.staff(4, 33)

  def runObscalcUpdate(p: Program.Id, o: Observation.Id): IO[Unit] =
    runObscalcUpdateAs(serviceUser, p, o)

  private def runProgramObscalc(pid: Program.Id): IO[Unit] =
    withSession: session =>
      session.execute(
        sql"""
          SELECT c_observation_id
          FROM t_obscalc
          WHERE c_program_id = $program_id
            AND c_obscalc_state IN ('pending', 'retry')
        """.query(observation_id)
      )(pid)
    .flatMap: oids =>
      oids.traverse_(oid => runObscalcUpdate(pid, oid))

  override val validUsers: List[User] =
    List(pi, pi2, serviceUser, staff)

  val createProgram: IO[Program.Id] =
    createProgramAs(pi, "Sequence Testing")

  def calibrationProgram(role: CalibrationRole): IO[Program.Id] =
    query(
      user = staff,
      query = s"""
        query {
          programs(
            WHERE: {
              calibrationRole: {
                EQ: ${role.tag.toScreamingSnakeCase}
              }
            }
          ) {
            matches {
              id
            }
          }
        }
      """
    ).flatMap: js =>
      js.hcursor
        .downFields("programs", "matches")
        .values.toList.flatMap(_.toList).head // grab the first / only match
        .hcursor
        .downField("id").as[Program.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  def twilightProgram: IO[Program.Id] =
    calibrationProgram(CalibrationRole.Twilight)

  def calibrationTargets(role: CalibrationRole): IO[List[Target.Id]] =
    calibrationProgram(role).flatMap: pid =>
      query(
        user = staff,
        query = s"""
          query {
            targets(
              WHERE: {
                program: {
                  id: { EQ: "$pid" }
                }
              }
            ) {
              matches {
                id
              }
            }
          }
        """
      ).flatMap: js =>
        js.hcursor
          .downFields("targets", "matches")
          .values.toList.flatMap(_.toList)
          .traverse(_.hcursor.downField("id").as[Target.Id])
          .leftMap(f => new RuntimeException(f.message))
          .liftTo[IO]

  def twilightTargets: IO[List[Target.Id]] =
    calibrationTargets(CalibrationRole.Twilight)

  def executionConfigQuery(oid: Observation.Id, inst: String, sequenceType: String, atomQuery: String, futureLimit: Option[Int]): String =
    s"""
      query {
        executionConfig(observationId: "$oid"${futureLimit.fold("")(lim => s", futureLimit: $lim")}) {
          $inst {
            $sequenceType {
              nextAtom {
                $atomQuery
              }
              possibleFuture {
                $atomQuery
              }
              hasMore
            }
          }
        }
      }
    """

  val ObsWavelength: Wavelength =
    Wavelength.decimalNanometers.unsafeGet(BigDecimal("500.0"))

  def obsWavelengthAt(ditherNm: Int): Wavelength =
    ObsWavelength.unsafeOffset(
      WavelengthDither.decimalNanometers.unsafeGet(BigDecimal(ditherNm))
    )

  def telescopeConfig(p: Int, q: Int, g: StepGuideState): TelescopeConfig =
    TelescopeConfig(
      Offset(
        Offset.P.signedDecimalArcseconds.reverseGet(BigDecimal(p)),
        Offset.Q.signedDecimalArcseconds.reverseGet(BigDecimal(q))
      ),
      g
    )

  def acqTelescopeConfig(p: Int): TelescopeConfig =
    telescopeConfig(p, 0, StepGuideState.Enabled)

  def gcalTelescopeConfig(q: Int): TelescopeConfig =
    telescopeConfig(0, q, StepGuideState.Disabled)

  def sciTelescopeConfig(q: Int): TelescopeConfig =
    telescopeConfig(0, q, StepGuideState.Enabled)

  protected def expectedTelescopeConfig(p: Int, q: Int, g: StepGuideState): Json =
    expectedTelescopeConfig(telescopeConfig(p, q, g))

  protected def expectedTelescopeConfig(t: TelescopeConfig): Json =
    json"""
      {
        "offset": {
          "p": { "arcseconds": ${Angle.signedDecimalArcseconds.get(t.offset.p.toAngle)} },
          "q": { "arcseconds": ${Angle.signedDecimalArcseconds.get(t.offset.q.toAngle)} }
        },
        "guiding": ${t.guiding}
      }
    """

  def addEndStepEvent(sid: Step.Id): IO[Unit] = {
    val q = s"""
      mutation {
        addStepEvent(input: {
          stepId: "$sid",
          stepStage: END_STEP
        }) {
          event {
            step {
              id
            }
          }
        }
      }
    """

    query(serviceUser, q).void
  }

  def addDatasetEvent(did: Dataset.Id, stage: DatasetStage): IO[Unit] = {
    val q = s"""
      mutation {
        addDatasetEvent(input: {
          datasetId:    "$did",
          datasetStage: ${stage.tag.toUpperCase}
        }) {
          event {
            id
          }
        }
      }
    """

    query(serviceUser, q).void
  }

  def setQaState(did: Dataset.Id, qa: DatasetQaState): IO[Unit] = {
    val q = s"""
        mutation {
          updateDatasets(input: {
            SET: {
              qaState: ${qa.tag.toUpperCase}
            },
            WHERE: {
              id: { EQ: "$did" }
            }
          }) {
            datasets {
              id
            }
          }
        }
    """

    query(serviceUser, q).void
  }

  /**
   * What time is it now, as a Timestamp.
   */
  def timestampNow: IO[Timestamp] =
    Clock[IO]
      .realTimeInstant
      .map(Timestamp.fromInstantTruncated)
      .flatMap(t => IO.fromOption(t)(new RuntimeException("oddly, timestamp of now is out of range")))

  def timeEstimateCalculator: IO[TimeEstimateCalculatorImplementation.ForInstrumentMode] =
    withSession: session =>
      Enums.load(session).flatMap: enums =>
        TimeEstimateCalculatorImplementation.fromSession(session, enums)

  /**
   * @return list of added and removed calibration observations
   */
  def recalculateCalibrations(pid: Program.Id, when: Instant): IO[(List[Observation.Id], List[Observation.Id])] =
    import Trace.Implicits.noop
    // First run obscalc for all pending observations in this program
    runProgramObscalc(pid) *>
      withServices(serviceUser): services =>
        for
          _ <- Services.asSuperUser(UserService.fromSession(services.session).canonicalizeUser(serviceUser))
          r <- services.transactionally:
                Services.asSuperUser:
                  services
                    .calibrationsService
                    .recalculateCalibrations(pid, when)
        yield r

  /**
   * Resolve all pending telluric targets for a program.
   * This processes the resolution queue synchronously for testing.
   */
  def resolveTelluricTargets: IO[Unit] =
    withServices(serviceUser) { services =>
      import Trace.Implicits.noop
      for
        _       <- Services.asSuperUser(UserService.fromSession(services.session).canonicalizeUser(serviceUser))
        pending <- services.session.transaction.use: _ =>
                     Services.asSuperUser(services.telluricTargetsService.load(100))
        _       <- pending.traverse_ : p =>
                     Services.asSuperUser(services.telluricTargetsService.resolveTargets(p))
      yield ()
    }
}

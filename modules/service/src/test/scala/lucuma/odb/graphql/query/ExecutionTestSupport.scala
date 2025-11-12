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
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.syntax.string.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.Generator
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services

import java.time.Instant

trait ExecutionTestSupport extends OdbSuite with ObservingModeSetupOperations {

  val pi: User          = TestUsers.Standard.pi(1, 30)
  val pi2: User         = TestUsers.Standard.pi(2, 32)
  val serviceUser       = TestUsers.service(3)
  val staff: User       = TestUsers.Standard.staff(4, 33)

  def runObscalcUpdate(p: Program.Id, o: Observation.Id): IO[Unit] =
    runObscalcUpdateAs(serviceUser, p, o)

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

  /**
   * Generates the sequence for the given observation.
   *
   * @param limit the future limit, which must be in the range [0, 100]
   * @param when the timestamp to pass the generator. in other words generate as
   *             if asked at this time
   */
  def generate(
    pid:      Program.Id,
    oid:      Observation.Id,
    limit:    Option[Int]       = None,  // [0, 100]
    when:     Option[Timestamp] = None
  ): IO[Either[OdbError, InstrumentExecutionConfig]] =
    withSession: session =>
      for
        future <- limit.traverse(lim => IO.fromOption(Generator.FutureLimit.from(lim).toOption)(new IllegalArgumentException("Specify a future limit from 0 to 100")))
        enums  <- Enums.load(session)
        tec    <- TimeEstimateCalculatorImplementation.fromSession(session, enums)
        srv     = servicesFor(serviceUser, enums)(session)
        gen     = srv.generator(CommitHash.Zero, tec)
        res    <- Services.asSuperUser(gen.generate(pid, oid, future.getOrElse(Generator.FutureLimit.Default), when))
      yield res

  /**
   * Generates the sequence but fails if it produces an error.
   *
   * @param limit the future limit, which must be in the range [0, 100]
   * @param when the timestamp to pass the generator. in other words generate as
   *             if asked at this time
   */
  def generateOrFail(
    pid:      Program.Id,
    oid:      Observation.Id,
    limit:    Option[Int]       = None,  // [0, 100]
    when:     Option[Timestamp] = None
  ): IO[InstrumentExecutionConfig] =
    generate(pid, oid, limit, when).flatMap: res =>
      IO.fromEither(res.leftMap(e => new RuntimeException(s"Failed to generate the sequence: ${e.message}")))

  /**
   * Generates the sequence as if requested after the specified amount of time
   * has passed.
   *
   * @param time amount of time (from now) to use as a timestamp for sequence
   *             generation; does not delay the computation in any way
   */
  def generateAfter(
    pid:  Program.Id,
    oid:  Observation.Id,
    time: TimeSpan
  ): IO[Either[OdbError, InstrumentExecutionConfig]] =
    for {
      now  <- timestampNow
      when <- IO.fromOption(now.plusMicrosOption(time.toMicroseconds))(new IllegalArgumentException(s"$time is too big"))
      res  <- generate(pid, oid, when = when.some)
    } yield res

  /**
   * Generates the sequence as if requested after the specified amount of time
   * has passed, fails if the sequence cannot be generated.
   *
   * @param time amount of time (from now) to use as a timestamp for sequence
   *             generation; does not delay the computation in any way
   */
  def generateAfterOrFail(
    pid:  Program.Id,
    oid:  Observation.Id,
    time: TimeSpan
  ): IO[InstrumentExecutionConfig] =
    generateAfter(pid, oid, time).flatMap: res =>
      IO.fromEither(res.leftMap(e => new RuntimeException(s"Failed to generate the sequence: ${e.message}")))

  /**
   * @return list of added and removed calibration observations
   */
  def recalculateCalibrations(pid: Program.Id, when: Instant): IO[(List[Observation.Id], List[Observation.Id])] =
    withServices(serviceUser): services =>
      services.session.transaction.use: xa =>
        Services.asSuperUser:
          services
            .calibrationsService
            .recalculateCalibrations(pid, when)(using xa)
}

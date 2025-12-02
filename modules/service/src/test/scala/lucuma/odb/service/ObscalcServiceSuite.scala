// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.SetupTime
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.util.CalculationState
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.data.Obscalc
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.calculation_state
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.program_id
import skunk.*
import skunk.implicits.*
import scala.collection.immutable.SortedSet

trait ObscalcServiceSuiteSupport extends ExecutionTestSupportForGmos:

  def withObscalcService[A](f: ServiceAccess ?=> ObscalcService[IO] => IO[A]): IO[A] =
    withServicesForObscalc(serviceUser): services =>
      given Services[IO] = services
      f(ObscalcService.instantiate[IO])

  def withObscalcServiceTransactionally[A](f: (ServiceAccess, Transaction[IO]) ?=> ObscalcService[IO] => IO[A]): IO[A] =
    withServicesForObscalc(serviceUser): services =>
      services.transactionally:
        f(ObscalcService.instantiate[IO])

  def select(o: Observation.Id): IO[Option[Obscalc.Entry]] =
    withObscalcServiceTransactionally(_.selectOne(o))

  def load: IO[List[Obscalc.PendingCalc]] =
    withObscalcServiceTransactionally(_.load(10))

  def calculateAndUpdate(p: Obscalc.PendingCalc): IO[Option[Obscalc.Meta]] =
    withObscalcService(_.calculateAndUpdate(p))

  def calculateOnly(pc: Obscalc.PendingCalc): IO[Obscalc.Result] =
    withObscalcService(_.calculateOnly(pc))

  def reset: IO[Unit] =
    withObscalcServiceTransactionally(_.reset)

  def insert(pc: Obscalc.PendingCalc): IO[Unit] =
    withSession: session =>
      val ins: Command[(Program.Id, Observation.Id, Timestamp)] = sql"""
        INSERT INTO t_obscalc (
          c_program_id,
          c_observation_id,
          c_last_invalidation
        ) SELECT
          $program_id,
          $observation_id,
          $core_timestamp
        ON CONFLICT ON CONSTRAINT t_obscalc_pkey DO UPDATE
          SET c_last_invalidation = $core_timestamp
      """.command.contramap((p, o, t) => (p, o, t, t))

      session.execute(ins)(pc.programId, pc.observationId, pc.lastInvalidation).void

  def setRetry(o: Observation.Id): IO[Unit] =
    withSession: session =>
      val up: Command[Observation.Id] = sql"""
        UPDATE t_obscalc
        SET c_obscalc_state = 'retry' :: e_calculation_state,
            c_retry_at      = now(),
            c_failure_count = 10
        WHERE c_observation_id = $observation_id
      """.command

      session.execute(up)(o).void

  def calculationState(o: Observation.Id): IO[CalculationState] =
    withSession: session =>
      val query = sql"""
        SELECT c_obscalc_state
        FROM t_obscalc
        WHERE c_observation_id = $observation_id
      """.query(calculation_state)
      session.unique(query)(o)

  val selectStates: IO[Map[Observation.Id, CalculationState]] =
    withSession: session =>
      val states: Query[Void, (Observation.Id, CalculationState)] = sql"""
        SELECT
          c_observation_id,
          c_obscalc_state
        FROM
          t_obscalc
      """.query(observation_id *: calculation_state)

      session.execute(states).map(_.toMap)

  val cleanup: IO[Unit] =
    withSession: session =>
      val truncate = sql"""
        TRUNCATE t_obscalc
      """.command
      session.execute(truncate).void

class ObscalcServiceSuite extends ObscalcServiceSuiteSupport:

  extension (s: String)
    def sec: BigDecimal =
      BigDecimal(s).setScale(6)

  val OneDataset =
    "10.0".sec + // exposure time
    "41.1".sec + // readout
    "10.0".sec   // writeout

  val CalTime =
    "15.0".sec +  // science fold
    "52.1".sec +  // arc
    "57.1".sec    // flat

  val Offset_15arcsec = "7.0".sec + "0.09375".sec
  val Offset_30arcsec = "7.0".sec + "0.18750".sec


  val Atom1 = CalTime + "15.0".sec + OneDataset + Offset_15arcsec + OneDataset
  val Atom2 = Atom1
  val Atom3 = CalTime + "15.0".sec + OneDataset + Offset_30arcsec + OneDataset

  val ScienceSequence = Atom1 + Atom2 + Atom3

  val setup: IO[(Program.Id, Target.Id, Observation.Id)] =
    for
      _ <- cleanup
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      // Set the acquisition ROI to CCD2_STAMP to match the previous default.
      _ <- query(
        user  = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                observingMode: {
                  gmosNorthLongSlit: {
                    acquisition: {
                      explicitRoi: CCD2_STAMP
                    }
                  }
                }
              }
            }) {
              observations {
                id
              }
            }
          }
        """
      )
    yield (p, t, o)

  def fakeWithTargetResult(tid: Target.Id): Obscalc.Result =
    Obscalc.Result.WithTarget(
      Obscalc.ItcResult(
        ItcService.TargetResult(
          tid,
          fakeItcSpectroscopyResult,
          none
        ),
        ItcService.TargetResult(
          tid,
          fakeItcImagingResult,
          Wavelength.fromIntPicometers(500000).map(fakeSignalToNoiseAt)
        )
      ),
      ExecutionDigest(
          SetupTime(
            TimeSpan.unsafeFromMicroseconds(960000000),
            TimeSpan.unsafeFromMicroseconds(300000000)
          ),
          SequenceDigest(
            ObserveClass.Acquisition,
            CategorizedTime(ChargeClass.Program -> TimeSpan.unsafeFromMicroseconds(219362500L)),
            SortedSet.from(List(
              TelescopeConfig(Offset.Zero, StepGuideState.Enabled),
              TelescopeConfig(Offset.microarcseconds.reverseGet(10000000L, 0L), StepGuideState.Enabled)
            )),
            NonNegInt.unsafeFrom(2),
            ExecutionState.NotStarted
          ),
          SequenceDigest(
            ObserveClass.Science,
            CategorizedTime(ChargeClass.Program -> TimeSpan.FromSeconds.getOption(ScienceSequence).get),
            SortedSet.from(List(
              TelescopeConfig(Offset.Zero, StepGuideState.Disabled),
              TelescopeConfig(Offset.Zero, StepGuideState.Enabled),
              TelescopeConfig(Offset.microarcseconds.reverseGet(0L, 15000000L), StepGuideState.Disabled),
              TelescopeConfig(Offset.microarcseconds.reverseGet(0L, 15000000L), StepGuideState.Enabled),
              TelescopeConfig(Offset.microarcseconds.reverseGet(0L, 1295985000000L), StepGuideState.Enabled)
            )),
            NonNegInt.unsafeFrom(3),
            ExecutionState.NotStarted
          )
      ),
      ObservationWorkflow(ObservationWorkflowState.Defined, List(ObservationWorkflowState.Inactive), Nil)
    )

  val randomTime = Timestamp.unsafeFromInstantTruncated(java.time.Instant.now)

  test("calc with error"):
    for
      p <- createProgram
      o <- createGmosNorthLongSlitObservationAs(pi, p, Nil)
      _ <- assertIO(
            calculateOnly(Obscalc.PendingCalc(p, o, randomTime)),
            Obscalc.Result.Error(
              OdbError.SequenceUnavailable(o, s"Could not generate a sequence for $o: observation is missing target".some),
              ObservationWorkflow(ObservationWorkflowState.Undefined, List(ObservationWorkflowState.Inactive), List(ObservationValidation.configuration("Missing target")))
            )
          )
    yield ()

  test("calc with target"):
    setup.flatTap: (p, t, o) =>
      assertIO(
        calculateOnly(Obscalc.PendingCalc(p, o, randomTime)),
        fakeWithTargetResult(t)
      )

  test("load"):
    setup.flatTap: (p, _, o) =>
       val pc = Obscalc.PendingCalc(p, o, randomTime)
       assertIO(insert(pc) *> load, List(pc)) *>
       assertIO(calculationState(o), CalculationState.Calculating)

  test("update then select"):
    setup.flatTap: (p, _, o) =>
      val pc = Obscalc.PendingCalc(p, o, randomTime)
      calculateOnly(pc).flatTap: r =>
        assertIO(
          insert(pc) *> calculateAndUpdate(pc) *> select(o).map(_.flatMap(_.result)),
          r.some
        )

  test("update with error then select"):
    for
      p <- createProgram
      o <- createGmosNorthLongSlitObservationAs(pi, p, Nil)
      pc = Obscalc.PendingCalc(p, o, randomTime)
      r <- calculateOnly(pc)
      _ <- insert(pc)
      _ <- calculateAndUpdate(pc)
      _ <- assertIO(select(o).map(_.flatMap(_.result)), r.some)
    yield ()

  test("select no result"):
    for
      p <- createProgram
      o <- createGmosNorthLongSlitObservationAs(pi, p, Nil)
      pc = Obscalc.PendingCalc(p, o, randomTime)
      _ <- insert(pc)
      _ <- assertIO(select(o).map(_.flatMap(_.result)), none)
    yield ()

  test("update then load"):
    (setup <* cleanup).flatTap: (p, _, o) =>
      val pc = Obscalc.PendingCalc(p, o, randomTime)
      assertIO(insert(pc) *> calculateAndUpdate(pc) *> load, Nil)

  test("invalidate, update then load"):
    setup.flatTap: (p, _, o) =>
      val pc  = Obscalc.PendingCalc(p, o, randomTime)
      val pc2 = pc.copy(lastInvalidation = randomTime.plusMicrosOption(1).get)
      assertIO(insert(pc2) *> calculateAndUpdate(pc) *> load, List(pc2))

  test("reset"):
    val states = for
      _  <- cleanup
      p  <- createProgram
      o0 <- createGmosNorthLongSlitObservationAs(pi, p, Nil)
      o1 <- createGmosNorthLongSlitObservationAs(pi, p, Nil)
      _  <- setRetry(o1)
      r0 <- selectStates
      _  <- load
      r1 <- selectStates
      _  <- reset
      r2 <- selectStates
    yield (List(r0(o0), r1(o0), r2(o0)),
           List(r0(o1), r1(o1), r2(o1)))

    assertIOBoolean:
      states.map: (o0, o1) =>
        o0 === List(CalculationState.Pending, CalculationState.Calculating, CalculationState.Pending) &&
        o1 === List(CalculationState.Retry,   CalculationState.Calculating, CalculationState.Retry  )

  test("mark failed"):
    def setWavelengthToMagicValue(o: Observation.Id): IO[Unit] =
      withSession: session =>
        val cmd = sql"""
          UPDATE t_gmos_north_long_slit
             SET c_central_wavelength = 666000
           WHERE c_observation_id = $observation_id
        """.command
        session.execute(cmd)(o).void

    val res = (setup <* cleanup).flatMap: (_, _, o) =>
      setWavelengthToMagicValue(o) *>
      load.flatMap: lst =>
        calculateAndUpdate(lst.head) *> selectStates

    assertIO(res.map(_.values.toList.head), CalculationState.Retry)

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order.catsKernelOrderingForOrder
import cats.effect.IO
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.SetupTime
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.data.Obscalc
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.obscalc_state
import lucuma.odb.util.Codecs.observation_id
import skunk.*
import skunk.implicits.*

import scala.collection.immutable.SortedSet

class ObscalcServiceSuite extends ExecutionTestSupport:

  val setup: IO[(Program.Id, Target.Id, Observation.Id)] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    yield (p, t, o)

  def instantiate(services: Services[IO]): IO[ObscalcService[IO]] =
      TimeEstimateCalculatorImplementation
        .fromSession(services.session, services.enums)
        .map: tec =>
          services.obscalcService(CommitHash.Zero, itcClient, tec)

  def withObscalcService[A](f: ObscalcService[IO] => IO[A]): IO[A] =
    withServices(serviceUser): services =>
      instantiate(services).flatMap(f)

  def withObscalcServiceTransactionally[A](f: Transaction[IO] ?=> ObscalcService[IO] => IO[A]): IO[A] =
    withServices(serviceUser): services =>
      services.transactionally:
        instantiate(services).flatMap(f)

  def select(o: Observation.Id): IO[Option[Obscalc]] =
    withObscalcServiceTransactionally(_.selectOne(o))

  val load: IO[List[Obscalc.PendingCalc]] =
    withObscalcServiceTransactionally(_.load(10))

  def calc(pc: Obscalc.PendingCalc): IO[Obscalc.Result] =
    withObscalcService(_.calculate(pc))

  def update(p: Obscalc.PendingCalc, r: Obscalc.Result): IO[Unit] =
    withObscalcServiceTransactionally(_.update(p, r))

  val cleanup: IO[Unit] =
    withSession: session =>
      val truncate = sql"""
        TRUNCATE t_obscalc
      """.command
      session.execute(truncate).void

  def insert(pc: Obscalc.PendingCalc): IO[Unit] =
    withSession: session =>
      val ins: Command[(Observation.Id, Timestamp)] = sql"""
        INSERT INTO t_obscalc (
          c_observation_id,
          c_last_invalidation
        ) SELECT
          $observation_id,
          $core_timestamp
        ON CONFLICT ON CONSTRAINT t_obscalc_pkey DO UPDATE
          SET c_last_invalidation = $core_timestamp
      """.command.contramap((o, t) => (o, t, t))

      session.execute(ins)(pc.observationId, pc.lastInvalidation).void

  def obscalcState(o: Observation.Id): IO[Obscalc.State] =
    withSession: session =>
      val query = sql"""
        SELECT c_obscalc_state
        FROM t_obscalc
        WHERE c_observation_id = $observation_id
      """.query(obscalc_state)
      session.unique(query)(o)

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
            SortedSet(Offset.Zero, Offset.microarcseconds.reverseGet(10000000L, 0L)),
            NonNegInt.unsafeFrom(2),
            ExecutionState.NotStarted
          ),
          SequenceDigest(
            ObserveClass.Science,
            CategorizedTime(ChargeClass.Program -> TimeSpan.unsafeFromMicroseconds(784200000L)),
            SortedSet(Offset.microarcseconds.reverseGet(0L, 1295985000000L), Offset.Zero, Offset.microarcseconds.reverseGet(0L, 15000000L)),
            NonNegInt.unsafeFrom(3),
            ExecutionState.NotStarted
          )
      )
    )

  val randomTime = Timestamp.unsafeFromInstantTruncated(java.time.Instant.now)

  test("calc with error"):
    for
      p <- createProgram
      o <- createGmosNorthLongSlitObservationAs(pi, p, Nil)
      _ <- assertIO(
            calc(Obscalc.PendingCalc(p, o, randomTime)),
            Obscalc.Result.Error(OdbError.SequenceUnavailable(s"Could not generate the $o sequence: observation is missing target".some))
          )
    yield ()

  test("calc with target"):
    setup.flatTap: (p, t, o) =>
      assertIO(
        calc(Obscalc.PendingCalc(p, o, randomTime)),
        fakeWithTargetResult(t)
      )

  test("load"):
    setup.flatTap: (p, _, o) =>
       val pc = Obscalc.PendingCalc(p, o, randomTime)
       assertIO(insert(pc) *> load, List(pc)) *>
       assertIO(obscalcState(o), Obscalc.State.Calculating)

  test("update then select"):
    setup.flatTap: (p, _, o) =>
      val pc = Obscalc.PendingCalc(p, o, randomTime)
      calc(pc).flatTap: r =>
        assertIO(
          insert(pc) *> update(pc, r) *> select(o).map(_.flatMap(_.result)),
          r.some
        )

  test("update with error then select"):
    for
      p <- createProgram
      o <- createGmosNorthLongSlitObservationAs(pi, p, Nil)
      pc = Obscalc.PendingCalc(p, o, randomTime)
      r <- calc(pc)
      _ <- insert(pc)
      _ <- update(pc, r)
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
    (cleanup *> setup).flatTap: (p, _, o) =>
      val pc = Obscalc.PendingCalc(p, o, randomTime)
      calc(pc).flatTap: r =>
        assertIO(insert(pc) *> update(pc, r) *> load, Nil)

  test("invalidate, update then load"):
    (cleanup *> setup).flatTap: (p, _, o) =>
      val pc = Obscalc.PendingCalc(p, o, randomTime)
      calc(pc).flatTap: r =>
        // invalidated before the result was written out
        val pc2 = pc.copy(lastInvalidation = randomTime.plusMicrosOption(1).get)
        assertIO(insert(pc2) *> update(pc, r) *> load, List(pc2))
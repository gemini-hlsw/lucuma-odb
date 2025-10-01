// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.syntax.option.*
import fs2.io.file.Files
import fs2.io.file.Path
import lucuma.catalog.votable.CatalogAdapter
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.odb.data.BlindOffset
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.http4s.headers.`Content-Type`
import skunk.*
import skunk.codec.numeric.int8
import skunk.implicits.*

trait BlindOffsetServiceSuiteSupport extends ExecutionTestSupportForGmos:

  // Override httpClient to return mock Gaia VOTable responses using real format from result.xml
  override protected def httpClient: Client[IO] =
    Client.fromHttpApp[IO](HttpApp[IO] { _ =>
      Files[IO]
        .readAll(Path("modules/service/src/test/scala/lucuma/catalog/votable/result.xml"))
        .through(fs2.text.utf8.decode)
        .compile
        .string
        .flatMap(content => Ok(content).map(_.withContentType(`Content-Type`(MediaType.application.xml))))
    })

  // Override gaiaAdapters to use GAVO adapter which matches the result.xml format
  override protected val gaiaAdapters: NonEmptyChain[CatalogAdapter.Gaia] =
    NonEmptyChain.one(CatalogAdapter.Gaia3LiteGavo)

  def instantiate(services: Services[IO]): IO[ObscalcService[IO]] =
    TimeEstimateCalculatorImplementation
      .fromSession(services.session, services.enums)
      .map: tec =>
        services.obscalcService(CommitHash.Zero, itcClient, tec)

  def withObscalcService[A](f: ServiceAccess ?=> ObscalcService[IO] => IO[A]): IO[A] =
    withServicesForObscalc(serviceUser): services =>
      instantiate(services).flatMap(f)

  def withObscalcServiceTransactionally[A](f: (ServiceAccess, Transaction[IO]) ?=> ObscalcService[IO] => IO[A]): IO[A] =
    withServicesForObscalc(serviceUser): services =>
      services.transactionally:
        instantiate(services).flatMap(f)

  def loadBlindOffsetCalcs(max: Int): IO[List[BlindOffset.PendingCalc]] =
    withObscalcServiceTransactionally(_.loadBlindOffsetCalcs(max))

  def calculateAndUpdateBlindOffset(pc: BlindOffset.PendingCalc): IO[Unit] =
    withObscalcService(_.calculateAndUpdateBlindOffset(pc))

  def insertBlindOffsetPending(pc: BlindOffset.PendingCalc): IO[Unit] =
    withSession: session =>
      val setObservation: Command[(Option[Angle], Option[Angle], Option[Timestamp], Observation.Id)] = sql"""
        UPDATE t_observation
        SET c_use_blind_offset = true,
            c_explicit_ra = ${angle_µas.opt},
            c_explicit_dec = ${angle_µas.opt},
            c_observation_time = ${core_timestamp.opt}
        WHERE c_observation_id = $observation_id
      """.command

      val ins: Command[(Program.Id, Observation.Id, Timestamp)] = sql"""
        INSERT INTO t_obscalc (
          c_program_id,
          c_observation_id,
          c_last_invalidation,
          c_obscalc_state,
          c_blind_offset_state
        ) SELECT
          $program_id,
          $observation_id,
          $core_timestamp,
          'ready' :: e_calculation_state,
          'pending' :: e_calculation_state
        ON CONFLICT ON CONSTRAINT t_obscalc_pkey DO UPDATE
          SET c_last_invalidation = $core_timestamp,
              c_obscalc_state = 'ready' :: e_calculation_state,
              c_blind_offset_state = 'pending' :: e_calculation_state
      """.command.contramap((p, o, t) => (p, o, t, t))

      println(pc.explicitBase)
      session.execute(setObservation)(
        pc.explicitBase.map(_.ra.toAngle),
        pc.explicitBase.map(_.dec.toAngle),
        pc.observationTime,
        pc.observationId
      ) *>
      session.execute(ins)(pc.programId, pc.observationId, pc.lastInvalidation).void

  def blindOffsetState(o: Observation.Id): IO[Option[CalculationState]] =
    withSession: session =>
      val query = sql"""
        SELECT c_blind_offset_state
        FROM t_obscalc
        WHERE c_observation_id = $observation_id
      """.query(calculation_state.opt)
      session.unique(query)(o)

  def getBlindOffsetTarget(o: Observation.Id): IO[Option[Target.Id]] =
    withSession: session =>
      val query = sql"""
        SELECT a.c_target_id
        FROM t_asterism_target a
        JOIN t_target t ON a.c_target_id = t.c_target_id
        WHERE a.c_observation_id = $observation_id
        AND t.c_target_disposition = 'blind_offset'
      """.query(target_id)
      session.option(query)(o)

  def getTargetCoordinates(t: Target.Id): IO[Option[Coordinates]] =
    withSession: session =>
      val query = sql"""
        SELECT c_sid_ra, c_sid_dec
        FROM t_target
        WHERE c_target_id = $target_id
      """.query(int8 *: int8)
      session.option(query)(t).map:
        _.map: (ra, dec) =>
          Coordinates(
            RightAscension.fromAngleExact.getOption(lucuma.core.math.Angle.microarcseconds.reverseGet(ra)).get,
            Declination.fromAngle.getOption(lucuma.core.math.Angle.microarcseconds.reverseGet(dec)).get
          )

  val cleanupBlindOffset: IO[Unit] =
    withSession: session =>
      val truncate = sql"""
        UPDATE t_obscalc
        SET c_blind_offset_state = NULL,
            c_blind_offset_manual_override = false
      """.command
      session.execute(truncate).void

class BlindOffsetServiceSuite extends BlindOffsetServiceSuiteSupport:

  val randomTime = Timestamp.unsafeFromInstantTruncated(java.time.Instant.now)

  val testBase: Coordinates = Coordinates(
    RightAscension.fromStringHMS.getOption("05:35:17.3").get,
    Declination.fromStringSignedDMS.getOption("+22:00:52.0").get
  )

  val setup: IO[(Program.Id, Target.Id, Observation.Id)] =
    for
      _ <- cleanupBlindOffset
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    yield (p, t, o)

  test("load pending blind offset calculations"):
    setup.flatTap: (p, _, o) =>
      val pc = BlindOffset.PendingCalc(p, o, randomTime, testBase.some, randomTime.some)
      assertIO(insertBlindOffsetPending(pc) *> loadBlindOffsetCalcs(10), List(pc)) *>
      assertIO(blindOffsetState(o), CalculationState.Calculating.some)

  test("load returns empty when no pending calculations"):
    setup.flatTap: (_, _, _) =>
      assertIO(loadBlindOffsetCalcs(10), Nil)

  test("load respects max limit"):
    for
      p  <- createProgram
      t  <- createTargetWithProfileAs(pi, p)
      o1 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      o2 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      o3 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      pc1 = BlindOffset.PendingCalc(p, o1, randomTime, testBase.some, randomTime.some)
      pc2 = BlindOffset.PendingCalc(p, o2, randomTime, testBase.some, randomTime.some)
      pc3 = BlindOffset.PendingCalc(p, o3, randomTime, testBase.some, randomTime.some)
      _  <- insertBlindOffsetPending(pc1)
      _  <- insertBlindOffsetPending(pc2)
      _  <- insertBlindOffsetPending(pc3)
      result <- loadBlindOffsetCalcs(2)
    yield assertEquals(result.length, 2)

  test("calculate and update creates blind offset target"):
    createUsers(serviceUser) *>
    setup.flatTap: (p, _, o) =>
      val pc = BlindOffset.PendingCalc(p, o, randomTime, testBase.some, randomTime.some)
      for
        _         <- insertBlindOffsetPending(pc)
        loaded    <- loadBlindOffsetCalcs(10)
        actualPc  =  loaded.find(_.observationId == o).get
        _         <- calculateAndUpdateBlindOffset(actualPc)
        targetOpt <- getBlindOffsetTarget(o)
        state     <- blindOffsetState(o)
      yield
        assert(targetOpt.isDefined, "Blind offset target should be created")
        // State is Pending because creating the blind offset target triggers asterism invalidation
        assertEquals(state, CalculationState.Pending.some)

  // test("calculate and update with explicit base"):
  //   setup.flatTap: (p, _, o) =>
  //     val pc = BlindOffset.PendingCalc(p, o, randomTime, Site.GN, testBase.some, randomTime.some)
  //     for
  //       _ <- insertBlindOffsetPending(pc)
  //       _ <- calculateAndUpdateBlindOffset(pc)
  //       targetOpt <- getBlindOffsetTarget(o)
  //     yield assert(targetOpt.isDefined, "Blind offset target should be created from explicit base")
  //
  // test("calculate updates existing blind offset target"):
  //   setup.flatTap: (p, _, o) =>
  //     val pc1 = BlindOffset.PendingCalc(p, o, randomTime, Site.GN, testBase.some, randomTime.some)
  //     val pc2 = BlindOffset.PendingCalc(p, o, randomTime.plusMicrosOption(1000).get, Site.GN, testBase.some, randomTime.some)
  //     for
  //       _ <- insertBlindOffsetPending(pc1)
  //       _ <- calculateAndUpdateBlindOffset(pc1)
  //       target1 <- getBlindOffsetTarget(o)
  //       coords1 <- target1.traverse(getTargetCoordinates)
  //       _ <- insertBlindOffsetPending(pc2)
  //       _ <- calculateAndUpdateBlindOffset(pc2)
  //       target2 <- getBlindOffsetTarget(o)
  //       coords2 <- target2.traverse(getTargetCoordinates)
  //     yield
  //       assert(target1.isDefined && target2.isDefined, "Target should exist after both calculations")
  //       assert(coords1.flatten.isDefined && coords2.flatten.isDefined, "Coordinates should be set")
  //
  // test("state transitions from pending to calculating to ready"):
  //   setup.flatTap: (p, _, o) =>
  //     val pc = BlindOffset.PendingCalc(p, o, randomTime, Site.GN, testBase.some, randomTime.some)
  //     for
  //       _ <- insertBlindOffsetPending(pc)
  //       state1 <- blindOffsetState(o)
  //       loaded <- loadBlindOffsetCalcs(10)
  //       state2 <- blindOffsetState(o)
  //       _ <- if loaded.nonEmpty then calculateAndUpdateBlindOffset(loaded.head) else IO.unit
  //       state3 <- blindOffsetState(o)
  //     yield
  //       assertEquals(state1, CalculationState.Pending.some)
  //       assertEquals(state2, CalculationState.Calculating.some)
  //       assertEquals(state3, CalculationState.Ready.some)

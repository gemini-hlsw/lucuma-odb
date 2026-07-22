// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Ref
import clue.ResponseException
import clue.model.GraphQLError
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.itc.ItcVersions
import lucuma.itc.client.ClientCalculationResult
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsResult
import lucuma.odb.data.Md5Hash
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.text.text
import skunk.implicits.*

import java.io.IOException

// Shared support for ITC service caching tests.
trait ItcServiceSuiteSupport extends ExecutionTestSupportForGmos:

  def erroringClient(err: Throwable, callCount: Ref[IO, Int]): ItcClient[IO] =
    new ItcClient[IO]:
      def spectroscopy(input: SpectroscopyInput, useCache: Boolean): IO[ClientCalculationResult] =
        callCount.update(_ + 1) *> IO.raiseError(err)

      def imaging(input: ImagingInput, useCache: Boolean): IO[ClientCalculationResult] =
        callCount.update(_ + 1) *> IO.raiseError(err)

      def spectroscopyIntegrationTimeAndGraphs(
        input: SpectroscopyIntegrationTimeAndGraphsInput, useCache: Boolean
      ): IO[SpectroscopyIntegrationTimeAndGraphsResult] =
        callCount.update(_ + 1) *> IO.raiseError(err)

      def versions: IO[ItcVersions] = IO.raiseError(err)

  def withItcService[A](client: ItcClient[IO])(f: ServiceAccess ?=> ItcService[IO] => IO[A]): IO[A] =
    withServicesForObscalc(serviceUser): services =>
      given Services[IO] = services
      f(ItcService.instantiate[IO](client))

  def itcFailureRowExists(pid: Program.Id, oid: Observation.Id): IO[Boolean] =
    withSession: s =>
      s.unique(
        sql"""
          SELECT count(*) FROM t_itc_result
          WHERE c_program_id      = $program_id      AND
                c_observation_id  = $observation_id  AND
                c_science_error  IS NOT NULL
        """.query(skunk.codec.numeric.int8)
      )(pid, oid).map(_ > 0)

  def itcRowExists(pid: Program.Id, oid: Observation.Id): IO[Boolean] =
    withSession: s =>
      s.unique(
        sql"""
          SELECT count(*) FROM t_itc_result
          WHERE c_program_id     = $program_id      AND
                c_observation_id = $observation_id
        """.query(skunk.codec.numeric.int8)
      )(pid, oid).map(_ > 0)

  def itcRowFrozen(pid: Program.Id, oid: Observation.Id): IO[Boolean] =
    withSession: s =>
      s.option(
        sql"""
          SELECT c_is_frozen FROM t_itc_result
          WHERE c_program_id     = $program_id AND
                c_observation_id = $observation_id
        """.query(bool)
      )(pid, oid).map(_.getOrElse(false))

  // Marks an observation's cached ITC result as frozen (as the freeze at
  // execution start would).
  def setFrozen(pid: Program.Id, oid: Observation.Id): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"""
          UPDATE t_itc_result
             SET c_is_frozen = true
           WHERE c_program_id     = $program_id AND
                 c_observation_id = $observation_id
        """.command
      )(pid, oid).void

  // Overwrites the stored input hash with a value that cannot match any real
  // input, so that only the frozen flag could explain a cache hit.
  def corruptHash(pid: Program.Id, oid: Observation.Id): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"""
          UPDATE t_itc_result
             SET c_hash = $md5_hash
           WHERE c_program_id     = $program_id AND
                 c_observation_id = $observation_id
        """.command
      )(Md5Hash.unsafeFromByteArray(Array.fill[Byte](16)(0)), pid, oid).void

  // Simulates an ITC version bump, which fires the itc_version_update trigger.
  def bumpItcVersion(version: String): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"""UPDATE t_itc_version SET c_version = $text""".command
      )(version).void

// Deterministic ITC failure test. It is for cases like sourec too bright or exposure time too long
class ItcServiceDeterministicFailureSuite extends ItcServiceSuiteSupport:

  test("deterministic failure is cached — second lookup skips remote ITC"):
    val deterministicErr = ResponseException[Unit](
      NonEmptyList.one(GraphQLError("The maximum exposure time is 600.0 seconds")),
      None
    )

    for
      callCount <- IO.ref(0)
      client     = erroringClient(deterministicErr, callCount)
      pid       <- createProgramAs(pi)
      tid       <- createTargetWithProfileAs(pi, pid)
      oid       <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _         <- withItcService(client): svc =>
                     for
                       r1    <- svc.lookup(pid, oid)
                       r2    <- svc.lookup(pid, oid)
                       calls <- callCount.get
                     yield
                       assert(r1.isLeft)
                       assertEquals(r1, r2)
                       // Second lookup must be served from DB, not from remote ITC.
                       assertEquals(calls, 1)
      exists    <- itcFailureRowExists(pid, oid)
    yield assert(exists) // a row added to the db

// Transient ITC failure: e.g. IOException
class ItcServiceTransientFailureSuite extends ItcServiceSuiteSupport:

  test("transient failure is NOT cached — remote ITC is called on every lookup"):
    for
      callCount <- IO.ref(0)
      client     = erroringClient(new IOException("Connection refused"), callCount)
      pid       <- createProgramAs(pi)
      tid       <- createTargetWithProfileAs(pi, pid)
      oid       <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _         <- withItcService(client): svc =>
                     for
                       r1    <- svc.lookup(pid, oid)
                       r2    <- svc.lookup(pid, oid)
                       calls <- callCount.get
                     yield
                       assert(r1.isLeft)
                       // Both lookups must hit the remote
                       assertEquals(calls, 2)
      exists    <- itcFailureRowExists(pid, oid)
    yield assert(!exists) // no row in the db

// Freezing: once execution begins the ITC result is durable and authoritative.
class ItcServiceFreezeSuite extends ItcServiceSuiteSupport:

  test("frozen ITC result survives an ITC version bump; an unfrozen one is wiped"):
    for
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      oidA <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      oidB <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      // Seed real successful results for both observations.
      _    <- withItcService(itcClient): svc =>
                svc.lookup(pid, oidA) *> svc.lookup(pid, oidB).void
      _    <- setFrozen(pid, oidA)
      _    <- bumpItcVersion("itc-freeze-test")
      a    <- itcRowExists(pid, oidA)
      b    <- itcRowExists(pid, oidB)
    yield
      assert(a, "frozen row should survive the version bump")
      assert(!b, "unfrozen cache row should be wiped by the version bump")

  test("frozen ITC result is served regardless of input hash and without calling remote"):
    for
      callCount <- IO.ref(0)
      pid  <- createProgramAs(pi)
      tid  <- createTargetWithProfileAs(pi, pid)
      oid  <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      // Seed a real success, then freeze it and destroy the hash so that only
      // the frozen flag could produce a cache hit.
      seed  <- withItcService(itcClient)(_.lookup(pid, oid))
      _     <- setFrozen(pid, oid)
      _     <- corruptHash(pid, oid)
      // Any remote call would fail (and increment the counter).
      got   <- withItcService(erroringClient(new IOException("Connection refused"), callCount))(_.lookup(pid, oid))
      calls <- callCount.get
    yield
      assert(seed.isRight, "seed lookup should succeed")
      assertEquals(got, seed) // frozen value served despite the hash mismatch
      assertEquals(calls, 0)  // remote ITC never consulted

  test("recording an observe visit freezes the observation's ITC result"):
    for
      pid    <- createProgramAs(pi)
      tid    <- createTargetWithProfileAs(pi, pid)
      oid    <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      before <- itcRowFrozen(pid, oid)
      _      <- recordVisitAs(serviceUser, oid)
      after  <- itcRowFrozen(pid, oid)
    yield
      assertEquals(before, false) // not frozen before execution begins
      assert(after, "the observe visit should freeze the ITC result")

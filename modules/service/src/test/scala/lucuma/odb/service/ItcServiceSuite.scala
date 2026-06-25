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
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.service.Services.ServiceAccess
import skunk.*
import skunk.implicits.*
import lucuma.odb.util.Codecs.*
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
          SELECT count(*) FROM t_itc_result_failure
          WHERE c_program_id = $program_id AND c_observation_id = $observation_id
        """.query(skunk.codec.numeric.int8)
      )(pid, oid).map(_ > 0)

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

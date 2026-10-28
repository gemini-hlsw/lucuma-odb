// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.telluric.TelluricSearchInput
import lucuma.catalog.telluric.TelluricStar
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.enums.TelluricSlot
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.TelluricType
import lucuma.core.syntax.timespan.*
import lucuma.core.util.CalculationState
import lucuma.odb.data.TelluricTargets
import lucuma.odb.graphql.TestUsers
import lucuma.odb.util.Codecs.*
import org.typelevel.otel4s.trace.Tracer
import skunk.*
import skunk.implicits.*

// A search result that lands after the resolution was invalidated must not touch
// the asterism: the telluric may have been converted to user-defined (and its star
// edited) in the meantime.
class TelluricStaleSearchSuite extends TelluricTargetsServiceSuiteSupport:

  override val pi = TestUsers.Standard.pi(1, 30)
  override val validUsers = List(pi)

  val star: TelluricStar =
    TelluricStar(
      id          = "HIP 1",
      spType      = TelluricType.A0V,
      coordinates = Coordinates.Zero,
      distance    = 1.0,
      hmag        = 7.0,
      score       = 1.0,
      slot       = TelluricSlot.After,
      sed         = None
    )

  override protected def telluricClient: IO[TelluricTargetsClient[IO]] =
    IO.pure:
      new TelluricTargetsClient[IO]:
        def search(input: TelluricSearchInput): IO[List[TelluricStar]] =
          IO.pure(List(star))
        def searchTarget(input: TelluricSearchInput): IO[List[(TelluricStar, Option[CatalogTargetResult])]] =
          IO.pure(List((star, None)))

  def asterism(oid: Observation.Id): IO[List[Target.Id]] =
    withSession: session =>
      val query: Query[Observation.Id, Target.Id] =
        sql"SELECT c_target_id FROM t_asterism_target WHERE c_observation_id = $observation_id".query(target_id)
      session.execute(query)(oid)

  def telluricTargetCount(pid: Program.Id): IO[Long] =
    withSession: session =>
      val query: Query[Program.Id, Long] =
        sql"SELECT count(*) FROM t_target WHERE c_program_id = $program_id AND c_calibration_role = 'telluric'".query(skunk.codec.all.int8)
      session.unique(query)(pid)

  // Linking a star records the acting user, so the service user must exist.
  def resolve(pending: TelluricTargets.Pending): IO[Option[TelluricTargets.Meta]] =
    withServices(serviceUser): services =>
      import Tracer.Implicits.noop
      Services.asSuperUser:
        UserService.fromSession(services.session).canonicalizeUser(serviceUser) *>
          services.telluricTargetsService.resolveTargets(pending)

  def setup: IO[(Program.Id, Observation.Id, TelluricTargets.Pending)] =
    for
      _       <- cleanup
      pid     <- createProgramAs(pi, "Telluric Stale Search Program")
      tid     <- createTargetWithProfileAs(pi, pid)
      sid     <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))
      oid     <- createTelluricCalibrationObservation(pi, pid)
      _       <- insertPending(createPendingEntry(pid, oid, sid, 30.minTimeSpan))
      pending <- loadObs(oid).map(_.get)
    yield (pid, oid, pending)

  test("a search that finishes in time links the star"):
    for
      (pid, oid, pending) <- setup
      meta                <- resolve(pending)
      row                 <- selectMeta(oid).map(_.get)
      targets             <- asterism(oid)
      count               <- telluricTargetCount(pid)
    yield
      assertEquals(meta, Some(row))
      assertEquals(row.state, CalculationState.Ready)
      assertEquals(targets, row.resolvedTargetId.toList)
      assertEquals(count, 1L)

  test("a search that finishes after a science edit links nothing and requeues"):
    for
      (pid, oid, pending) <- setup
      _                   <- withTelluricTargetsServiceTransactionally(_.updateScienceDuration(oid, 60.minTimeSpan))
      meta                <- resolve(pending)
      state               <- calculationState(oid)
      targets             <- asterism(oid)
      count               <- telluricTargetCount(pid)
    yield
      assertEquals(meta, None)
      assertEquals(state, CalculationState.Pending)
      assertEquals(targets, Nil)
      assertEquals(count, 0L)

  test("reset sends a calculating row back to pending"):
    for
      (_, oid, _) <- setup
      _           <- reset
      state       <- calculationState(oid)
    yield assertEquals(state, CalculationState.Pending)

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package scheduler

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.GcalLampType
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepType
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.AtomDigest
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.service.Services
import natchez.Trace.Implicits.noop
import org.http4s.*
import org.http4s.implicits.*

class schedulerRoutes extends SchedulerRoutesSuite with ExecutionTestSupportForGmos:

  val Zero: Atom.Id = Atom.Id.parse("a-00000000-0000-0000-0000-000000000000").get

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  def withRoutes[A](user: User, request: Request[IO]): IO[Response[IO]] =
    withSession: s =>
      Enums.load(s).flatMap: enums =>
        val srv = Services.forUser(user, enums, None)(s)
        SchedulerRoutes(srv, ssoClient).orNotFound.run(request)

  test("not service user"):
    atomsRequest(pi).flatMap: request =>
      withRoutes(serviceUser, request).assertResponse(Status.Forbidden, none)

  test("no observation ids"):
    atomsRequest(serviceUser).flatMap: request =>
      withRoutes(serviceUser, request).assertResponse(Status.Ok, "".some)

  test("invalid observation ids"):
    atomsRequest(serviceUser, "foo", "o-123", "bar").flatMap: request =>
      withRoutes(serviceUser, request).assertResponse(Status.BadRequest, "Unable to parse observation ids: foo, bar".some)

  test("unknown observation ids"):
    atomsRequest(serviceUser, "o-123", "o-124", "o-125").flatMap: request =>
      withRoutes(serviceUser, request).assertResponse(Status.Ok, "".some)

  val a0 =
    AtomDigest(
      Zero,  // we skip the atom id comparison
      ObserveClass.Science,
      CategorizedTime(ChargeClass.Program -> TimeSpan.unsafeFromMicroseconds(3892500000L)),
      Set(StepType.Gcal, StepType.Science),
      Set(GcalLampType.Arc, GcalLampType.Flat)
    )

  val a3 = a0.copy(timeEstimate = CategorizedTime(ChargeClass.Program -> TimeSpan.unsafeFromMicroseconds(1390300000L)))

  test("one observation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      atomsRequest(serviceUser, oid.toString).flatMap: request =>
        withRoutes(serviceUser, request).assertUncompressedAtoms(Status.Ok, List(oid -> List(a0, a0, a0, a3)))

  test("two observations"):
    val setup: IO[(Observation.Id, Observation.Id)] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o0 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _  <- runObscalcUpdate(p, o0)
        o1 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _  <- runObscalcUpdate(p, o1)
      yield (o0, o1)

    setup.flatMap: (oid0, oid1) =>
      atomsRequest(serviceUser, oid0.toString, oid1.toString).flatMap: request =>
        withRoutes(serviceUser, request).assertUncompressedAtoms(Status.Ok, List(oid0 -> List(a0, a0, a0, a3), oid1 -> List(a0, a0, a0, a3)))

  test("gzip"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      gzipAtomsRequest(serviceUser, gzip = true, oid.toString).flatMap: request =>
        withRoutes(serviceUser, request).assertCompressedAtoms(Status.Ok, List(oid -> List(a0, a0, a0, a3)))

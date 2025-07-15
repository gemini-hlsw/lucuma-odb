// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package scheduler

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.User
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.service.Services
import natchez.Trace.Implicits.noop
import org.http4s.*
import org.http4s.implicits.*

class schedulerRoutes extends SchedulerRoutesSuite:

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
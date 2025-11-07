// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.Parallel
import cats.data.NonEmptyList
import cats.effect.*
import cats.effect.std.SecureRandom
import cats.effect.std.UUIDGen
import cats.implicits.*
import fs2.compression.Compression
import lucuma.core.model.Access
import lucuma.core.model.AccessControlException
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.AtomDigest
import lucuma.core.syntax.string.*
import lucuma.core.util.Gid
import lucuma.core.util.Uid
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.sso.client.SsoClient
import natchez.Trace
import org.http4s.*
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Accept-Encoding`
import org.typelevel.ci.*
import org.typelevel.log4cats.Logger
import skunk.Session

object SchedulerRoutes:

  // the normal constructor
  def apply[F[_]: Async: Logger: Parallel: Trace: SecureRandom](
    pool:        Resource[F, Session[F]],
    ssoClient:   SsoClient[F, User],
    enums:       Enums,
    emailConfig: Config.Email,
    httpClient:  Client[F],
    itcClient:   ItcClient[F],
    gaiaClient:  lucuma.catalog.clients.GaiaClient[F]
  ): HttpRoutes[F] =
    apply(
      [A] => (u: User) => (fa: Services[F] => F[A]) => pool.map(Services.forUser(u, enums, None, emailConfig, httpClient, itcClient, gaiaClient, throw new RuntimeException("s3FileService not available in SchedulerRoutes"))).use(fa),
      ssoClient
    )

  // used by tests
  def apply[F[_]: Async](
    services:  Services[F],
    ssoClient: SsoClient[F, User]
  ): HttpRoutes[F] =
    apply(
      [A] => (_: User) => (fa: Services[F] => F[A]) => fa(services),
      ssoClient
    )

  private def tsv(row: (Observation.Id, Short, AtomDigest)): String =
    List(
      Gid[Observation.Id].show(row._1),
      row._2.toString,
      Uid[Atom.Id].show(row._3.id),
      row._3.observeClass.tag.toScreamingSnakeCase,
      row._3.timeEstimate.sum.toMicroseconds.toString,
      row._3.stepTypes.map(_.tag.toScreamingSnakeCase).mkString("[", ", ", "]"),
      row._3.lampTypes.map(_.tag.toScreamingSnakeCase).mkString("[", ", ", "]")
    ).intercalate("\t")

  def apply[F[_]: Async](
    services:  [A] => User => (Services[F] => F[A]) => F[A],
    ssoClient: SsoClient[F, User]
  ): HttpRoutes[F] =
    val dsl = Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F]:
      case req @ POST -> Root / "scheduler" / "atoms" =>
        val input: F[Either[NonEmptyList[String], List[Observation.Id]]] =
          req
            .body
            .through(fs2.text.utf8.decode)
            .through(fs2.text.lines)
            .filter(_.nonEmpty)
            .map(s => Observation.Id.parse(s).toRight(s).toValidatedNel)
            .compile
            .toList
            .map(_.sequence.toEither)

        for
          oids     <- input
          response <- oids.fold(
            errs     =>
              BadRequest(s"Unable to parse observation ids: ${errs.intercalate(", ")}"),
            validIds =>
              ssoClient.require(req) { user =>
                user.verifyAccess(Access.Service) *>
                services(user): s =>
                  Services.asSuperUser:
                    s.transactionally:

                      val rawStream =
                        sequenceService
                          .selectAtomDigests(validIds)
                          .map(tsv)
                          .intersperse("\n")
                          .through(fs2.text.utf8.encode)

                      val gzip: Boolean =
                        req
                          .headers
                          .get[`Accept-Encoding`]
                          .exists(_.values.exists(_ === ContentCoding.gzip))

                      if gzip then
                        val compressedStream =
                          rawStream
                            .through(Compression.forSync[F].gzip(deflateLevel = 9.some))
                        Ok(compressedStream).map: resp =>
                          resp.putHeaders(Header.Raw(ci"Content-Encoding", "gzip"))
                      else
                        Ok(rawStream)
              }.handleErrorWith:
                 case _: AccessControlException => Forbidden()
                 case other                     => other.raiseError[F, Response[F]]
          )
        yield response
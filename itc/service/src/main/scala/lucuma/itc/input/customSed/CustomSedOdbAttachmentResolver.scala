// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input.customSed

import cats.effect.Resource
import cats.effect.kernel.Async
import cats.syntax.all.*
import fs2.io.net.Network
import fs2.text
import lucuma.core.model.Attachment
import natchez.Trace
import org.http4s.AuthScheme
import org.http4s.Credentials
import org.http4s.Headers
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.Authorization
import org.typelevel.log4cats.Logger

/**
 * Implementation of `CustomSed.Resolver` that uses a URL to fetch the custom SED.
 */
class CustomSedOdbAttachmentResolver[F[_]: Async: Logger: Trace] private (
  client:     Client[F],
  odbBaseUrl: Uri,
  authToken:  String
) extends CustomSedDatResolver[F]:

  protected def datLines(id: Attachment.Id): F[fs2.Stream[F, String]] =
    Trace[F].span("custom-sed-attachment-resolver"):
      val uri: Uri            =
        odbBaseUrl / "attachment" / id.show
      val request: Request[F] =
        Request(
          uri = uri,
          headers = Headers:
            Authorization:
              Credentials.Token(AuthScheme.Bearer, authToken)
        )

      for
        _ <- Trace[F].put("custom-sed-attachment-resolver.uri" -> uri.toString)
        _ <- Logger[F].info(s"Fetching custom SED for id [$id]: $uri")
      yield client
        .stream(request)
        .flatMap(_.body)
        .through(text.utf8.decode)
        .through(text.lines)

object CustomSedOdbAttachmentResolver:
  def apply[F[_]: Async: Network: Logger: Trace](
    odbBaseUrl: Uri,
    authToken:  String
  ): Resource[F, CustomSed.Resolver[F]] =
    EmberClientBuilder
      .default[F]
      .build
      .map(client => new CustomSedOdbAttachmentResolver(client, odbBaseUrl, authToken))

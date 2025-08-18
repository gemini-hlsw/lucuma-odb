// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.orcid

import cats.effect.Concurrent
import cats.implicits.*
import natchez.Trace
import orcid.lucuma.sso.service.orcid.OrcidException
import org.http4s.*
import org.http4s.Uri.Authority
import org.http4s.Uri.Host
import org.http4s.Uri.Path
import org.http4s.Uri.Scheme
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.headers.Accept
import org.http4s.headers.Authorization

trait OrcidService[F[_]] {

  /**
   * Construct a URI for redirecting unauthenticated users to ORCID for authentication. ORCID will
   * redirect the user to `redirect` afterward, passing the authentication code on success or an
   * error message on failure, along with the `state` value, if any. On success, pass `redirect`
   * and the code to `getAccessToken` to complete authentication.
   */
  def authenticationUri(redirect: Uri, state: Option[String] = None): F[Uri]

  /**
   * Construct a logout URI. Visiting this URI will remove the user's persistent ORCID cookie. If
   * `script` is provided then the logout page will execute it, so this can be used for a
   * client-side redirect after logout, for example. This is weird but it's what ORCID provides.
   */
  def logoutUri(script: Option[String]): F[Uri]

  /**
   * Given a redirect (the same one that was passed to `authenticationUri`) and a resulting
   * authentication code, request an ORCID access token. The resulting structure contains the
   * user's ORCID iD, name, access token, and other useful things.
   */
  def getAccessToken(redirect: Uri, authenticationCode: String): F[OrcidAccess]

  /**
   * Retrieve information from the `person` record associated with the given `OrcidAccess`.
   */
  def getPerson(access: OrcidAccess): F[OrcidPerson]

}

object OrcidService {

  def apply[F[_]: Concurrent: Trace](
    orcidHost:         Host,
    orcidClientId:     String,
    orcidClientSecret: String,
    httpClient:        Client[F],
  ): OrcidService[F] =
    new OrcidService[F] with Http4sClientDsl[F] {

      def orcidUri(path: String): Uri =
        Uri(
          scheme    = Some(Scheme.https),
          authority = Some(Authority(host = orcidHost)),
          path      = Path.unsafeFromString(path) // hm
        )

      def logoutUri(script: Option[String]): F[Uri] =
        orcidUri("/userStatus.json")
          .withQueryParams(
            Map("logUserOut" -> "true") ++ script.foldMap(s =>
            Map("callback"   -> s))
          ).pure[F]

      def authenticationUri(redirect: Uri, state: Option[String]): F[Uri] =
        orcidUri("/oauth/authorize")
          .withQueryParams(
            state.foldMap(s => Map(
              "state"         -> s
            )) ++ Map(
            "client_id"     -> orcidClientId,
            "response_type" -> "code",
            "scope"         -> "/authenticate",
            "redirect_uri"  -> redirect.toString,
          )).pure[F]

      def getAccessToken(redirect: Uri, authenticationCode: String): F[OrcidAccess] =
        Trace[F].span("getAccessToken") {
          httpClient.expectOr(
            Method.POST(
              UrlForm(
                "client_id"     -> orcidClientId,
                "client_secret" -> orcidClientSecret,
                "grant_type"    -> "authorization_code",
                "redirect_uri"  -> redirect.toString,
                "code"          -> authenticationCode,
              ),
              orcidUri("/oauth/token"),
              Accept(MediaType.application.json)
            )
          )(_.as[OrcidException].widen)
        }

      def getPerson(access: OrcidAccess): F[OrcidPerson] =
        Trace[F].span("getPerson") {
          Trace[F].put("orcidId" -> access.orcidId.value) *>
          httpClient.expectOr[OrcidPerson](
            Method.GET(
              Uri.unsafeFromString(s"https://pub.$orcidHost/v3.0/${access.orcidId.value}/person"), // safe, heh-heh
              Accept(MediaType.application.json),
              Authorization(Credentials.Token(AuthScheme.Bearer, access.accessToken.toString))
            )
          )(_.as[OrcidException].widen)
        }

    }

}

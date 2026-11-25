// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.Applicative
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.sso.client.SsoClient.MaxLogLength
import org.typelevel.log4cats.Logger

/**
 * The WARN line for an authenticated user that an operation refuses. The GraphQL, chown and
 * attachment routes all use it, so the lines have one format.
 */
object NotAuthorizedLog:

  /** The line for `user`, refused `operation` for `reason`. The operation and the reason are truncated. */
  def line(user: User.Id, operation: String, reason: String): String =
    s"Not authorized: user $user, operation ${operation.take(MaxLogLength)}: ${reason.take(MaxLogLength)}"

  /** Logs the line for `user`, refused `operation` for `reason`. */
  def warn[F[_]: Logger](user: User.Id, operation: String, reason: String): F[Unit] =
    Logger[F].warn(line(user, operation, reason))

  /**
   * The line for the `NotAuthorized` problems of `result`, if there are any. The line names the
   * first problem, and gives the count of the others.
   */
  def fromResult(operation: => String, result: Result[?]): Option[String] =
    result.toProblems.toList.flatMap(p => p.asOdbError.collect { case OdbError.NotAuthorized(u, _) => (u, p.message) }) match
      case Nil                         => none
      case all @ ((user, reason) :: _) =>
        val more = if all.length > 1 then s" (and ${all.length - 1} more)" else ""
        s"${line(user, operation, reason)}$more".some

  /** Logs the line for the `NotAuthorized` problems of `result`, and tells if there was one. */
  def warnResult[F[_]: {Applicative, Logger}](operation: => String, result: Result[?]): F[Boolean] =
    fromResult(operation, result).traverse(Logger[F].warn(_)).map(_.isDefined)

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.Applicative
import cats.syntax.all.*
import grackle.Cursor
import grackle.Env
import grackle.Query
import grackle.QueryCompiler.Elab
import grackle.Result
import lucuma.common.middleware.UserContext
import lucuma.core.model.User

/**
 * Access to the user of the current request, which the routes supply in the Grackle `Env` under
 * `UserContext.EnvKey`. The mapping is built once for the whole server, so the user is read
 * per request rather than captured when the mapping is constructed.
 */
trait UserEnv:

  /** The user of the current request. */
  protected inline def user(using u: User): User = u

  /** Reads the user from the env and elaborates with it. */
  protected def withUser(f: User ?=> Elab[Unit]): Elab[Unit] =
    UserEnv.elab(u => f(using u))

object UserEnv:

  /** Reads the user from the env and elaborates with it. */
  def elab(f: User => Elab[Unit]): Elab[Unit] =
    Elab.envE[User](UserContext.EnvKey).flatMap(f)

  /** Reads the user from an env. */
  def fromEnv(env: Env): Result[User] =
    env.getR[User](UserContext.EnvKey)

  /** Reads the user from the env of an effect handler's queries, which share one request. */
  def fromQueries(queries: List[(Query, Cursor)]): Result[User] =
    queries
      .headOption
      .fold(Result.internalError[User]("No queries in effect handler."))((_, cursor) => cursor.envR[User](UserContext.EnvKey))

  /** Runs `f` with the user, if the user was read successfully. */
  def traverse[G[_]: Applicative, A](user: Result[User])(f: User ?=> G[Result[A]]): G[Result[A]] =
    user.flatTraverse(u => f(using u))

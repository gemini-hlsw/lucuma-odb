// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Applicative
import cats.data.NonEmptyChain
import cats.syntax.all.*
import clue.model.GraphQLError
import grackle.Problem
import grackle.Result
import io.circe.JsonObject
import io.circe.syntax.*

object OdbErrorExtensions:
  import OdbError.Key

  /** Some server-only extensions. */
  extension (e: OdbError)

    def asProblem: Problem =
      Problem(e.message, Nil, Nil, Some(JsonObject(Key -> e.asJson)))

    def asProblemNec: NonEmptyChain[Problem] =
      NonEmptyChain(asProblem)

    def asFailure: Result[Nothing] =
      Result.failure(asProblem)

    def asFailureF[F[_]: Applicative, A]: F[Result[A]] =
      asFailure.pure[F]

    def asWarning[A](a: A): Result[A] =
      Result.warning(asProblem, a)

    def asWarningF[F[_]: Applicative, A](a: A): F[Result[A]] =
      asWarning(a).pure[F]

  /** A client-side extension to recover an OdbError from a Clue GraphQLError. */
  extension (self: OdbError.type)

    def fromGraphQLError(gqe: GraphQLError): Option[OdbError] =
      for
        ext <- gqe.extensions
        obj <- ext(Key)
        err <- obj.as[OdbError].toOption
      yield err

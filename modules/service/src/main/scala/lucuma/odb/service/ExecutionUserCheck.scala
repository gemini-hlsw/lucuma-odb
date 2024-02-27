// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.either.*
import grackle.Result
import lucuma.core.model.Access.Service
import lucuma.core.model.User
import lucuma.odb.OdbError
import lucuma.odb.OdbErrorExtensions.*

import Services.Syntax.*

trait ExecutionUserCheck {

  def checkUser[F[_], A](f: User => A)(using Services[F]): Either[A, Unit] =
    user.role.access match {
      case Service => ().asRight // TODO: should specifically be the observe service user, whose name should be passed in the app config
      case _       => f(user).asLeft
    }

  def checkUser2[F[_]](using Services[F]): Result[Unit] =
    user.role.access match {
      case Service => Result.unit // TODO: should specifically be the observe service user, whose name should be passed in the app config
      case _       => OdbError.NotAuthorized(user.id).asFailure
    }

}

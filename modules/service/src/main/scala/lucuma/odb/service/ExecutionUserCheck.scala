// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.either.*
import lucuma.core.model.Access.Service
import lucuma.core.model.User

import Services.Syntax.*

trait ExecutionUserCheck {

  def checkUser[F[_], A](f: User => A)(using Services[F]): Either[A, Unit] =
    user.role.access match {
      case Service => ().asRight // TODO: should specifically be the observe service user, whose name should be passed in the app config
      case _       => f(user).asLeft
    }

}

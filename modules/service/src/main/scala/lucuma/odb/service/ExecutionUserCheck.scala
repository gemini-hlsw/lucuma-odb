// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.either.*
import lucuma.core.model.Access.Admin
import lucuma.core.model.Access.Service
import lucuma.core.model.Access.Staff
import lucuma.core.model.User

import Services.Syntax.*

trait ExecutionUserCheck {

  def checkUser[F[_], A](f: User => A)(using Services[F]): Either[A, Unit] =
    user.role.access match {
      case Admin | Service | Staff => ().asRight
      case _                       => /*f(user).asLeft*/ ().asRight // for now
    }

}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.Eq

import java.util.UUID

final case class SessionToken(value: UUID)
object SessionToken {

  implicit val EqSessionToken: Eq[SessionToken] =
    Eq.fromUniversalEquals

}
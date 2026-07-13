// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

import cats.syntax.show.*
import lucuma.core.model.User
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attributes

/** OpenTelemetry attributes describing an authenticated user, shared across services. */
object UserAttributes:

  val UserIdKey: AttributeKey[String]      = AttributeKey("user.id")
  val UserAccessKey: AttributeKey[String]  = AttributeKey("user.access")
  val UserDisplayKey: AttributeKey[String] = AttributeKey("user.displayName")

  /** Summarizes a user as a set of trace attributes. */
  given userAttributes: Attributes.Make[User] = u =>
    Attributes(
      Attribute(UserIdKey, u.id.show),
      Attribute(UserAccessKey, u.role.access.tag),
      Attribute(UserDisplayKey, u.displayName)
    )

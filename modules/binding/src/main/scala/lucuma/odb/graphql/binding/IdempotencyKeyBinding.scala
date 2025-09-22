// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.util.IdempotencyKey

import java.util.UUID
import scala.util.Try

val IdempotencyKeyBinding: Matcher[IdempotencyKey] =
  StringBinding.emap: s =>
    Try(UUID.fromString(s))
      .toOption
      .toRight(s"'$s' is not a valid UUID")
      .map(IdempotencyKey.apply)
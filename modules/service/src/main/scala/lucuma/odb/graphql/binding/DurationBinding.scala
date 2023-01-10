// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.syntax.either.*

import java.time.Duration
import java.time.format.DateTimeParseException

val DurationBinding: Matcher[Duration] =
  StringBinding.emap { s =>
    Either
      .catchOnly[DateTimeParseException](Duration.parse(s))
      .leftMap(_ => s"Invalid ISO-8601 duration: $s")
  }
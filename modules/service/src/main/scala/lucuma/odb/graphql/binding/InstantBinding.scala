// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.syntax.either.*

import java.time.Instant
import java.time.format.DateTimeParseException
import eu.timepit.refined

val InstantBinding: Matcher[Instant] =
  StringBinding.emap { s =>
    Either
      .catchOnly[DateTimeParseException](Instant.parse(s))
      .leftMap(_ => s"Invalid ISO-8601 instant: $s")
  }
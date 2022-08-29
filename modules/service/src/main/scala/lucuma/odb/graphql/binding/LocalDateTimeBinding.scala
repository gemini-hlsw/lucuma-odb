// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.syntax.either.*

import java.time.LocalDateTime
import java.time.format.DateTimeParseException

val LocalDateTimeBinding: Matcher[LocalDateTime] =
  StringBinding.emap { s =>
    Either
      .catchOnly[DateTimeParseException](LocalDateTime.parse(s))
      .leftMap(_ => s"Invalid ISO-8601 local date time: $s")
  }
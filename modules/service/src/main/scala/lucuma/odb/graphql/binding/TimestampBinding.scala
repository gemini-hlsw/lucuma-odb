// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.odb.data.Timestamp

import java.time.Instant
import java.time.format.DateTimeParseException

val TimestampBinding: Matcher[Timestamp] =
  LocalDateTimeBinding.emap { ldt =>
    Timestamp
      .FromLocalDateTime
      .getOption(ldt)
      .toRight(s"Timestamp out of range: $ldt")
  }
// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import java.time.LocalDate
import scala.util.control.Exception.nonFatalCatch

val DateBinding: Matcher[LocalDate] =
  StringBinding.emap { s =>
    nonFatalCatch
      .opt(LocalDate.parse(s))
      .toRight(s"Invalid date '$s', use ISO 8601 format: YYYY-MM-DD")
  }
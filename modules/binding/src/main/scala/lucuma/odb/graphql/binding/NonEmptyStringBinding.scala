// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import eu.timepit.refined.types.string.NonEmptyString

val NonEmptyStringBinding: Matcher[NonEmptyString] =
  StringBinding.emap { s =>
    NonEmptyString.unapply(s).toRight("string value must be non-empty.")
  }

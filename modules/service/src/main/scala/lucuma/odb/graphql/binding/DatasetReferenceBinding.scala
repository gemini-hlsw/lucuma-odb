// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.odb.data.DatasetReference

val DatasetReferenceBinding: Matcher[DatasetReference] =
  StringBinding.emap { s =>
    DatasetReference
      .fromString
      .getOption(s)
      .toRight(s"'$s' cannot be parsed as a valid DatasetReference.")
  }

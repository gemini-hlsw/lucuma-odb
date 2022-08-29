// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.odb.data.Timestamp

val TimestampBinding: Matcher[Timestamp] =
  InstantBinding.emap { inst =>
    Timestamp
      .FromInstant
      .getOption(inst)
      .toRight(s"Timestamp out of range: $inst")
  }
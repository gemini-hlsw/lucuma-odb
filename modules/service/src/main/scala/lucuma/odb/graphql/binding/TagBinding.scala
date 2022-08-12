// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.odb.data.Tag
import lucuma.odb.graphql.util.Bindings._

val TagBinding =
  TypedEnumBinding.map(v => Tag(v.name.toLowerCase))

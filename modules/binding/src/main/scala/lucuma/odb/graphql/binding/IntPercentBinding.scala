// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import grackle.Result
import lucuma.core.model.IntPercent

val IntPercentBinding: Matcher[IntPercent] =
  IntBinding.rmap(n => Result.fromEither(IntPercent.from(n)))

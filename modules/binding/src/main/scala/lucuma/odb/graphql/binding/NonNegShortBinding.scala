// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import eu.timepit.refined.types.numeric.NonNegShort

val NonNegShortBinding: Matcher[NonNegShort] =
  ShortBinding.emap(NonNegShort.from)

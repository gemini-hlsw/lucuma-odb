// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosBigDecimal

val PosBigDecimalBinding: Matcher[PosBigDecimal] =
  BigDecimalBinding.emap { s =>
    PosBigDecimal.from(s).leftMap(m => s"Invalid PosBigDecimal: $s: $m")
  }

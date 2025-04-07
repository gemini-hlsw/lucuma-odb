// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt

val PosIntBinding: Matcher[PosInt] =
  IntBinding.emap { i =>
    PosInt.from(i).leftMap(m => s"Invalid PosInt: $i: $m")
  }

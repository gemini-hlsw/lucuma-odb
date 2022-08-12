// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.syntax.all._
import lucuma.core.math.Epoch
import lucuma.odb.graphql.util.Bindings._

val EpochBinding: Matcher[Epoch] =
  StringBinding.emap { s =>
    Epoch.fromString.getOption(s).toRight(s"Invalid epoch: $s")
  }

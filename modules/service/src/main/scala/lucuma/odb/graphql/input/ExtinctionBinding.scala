// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import lucuma.odb.data.Extinction
import lucuma.odb.graphql.binding.FloatBinding

val ExtinctionBinding =
  FloatBinding.emap { d =>
    Extinction.FromMags.getOption(d).toRight(s"Invalid Extinction: $d")
  }
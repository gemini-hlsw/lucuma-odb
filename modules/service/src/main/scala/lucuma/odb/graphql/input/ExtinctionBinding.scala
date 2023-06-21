// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import lucuma.odb.graphql.binding.FloatBinding
import lucuma.odb.data.Extinction

val ExtinctionBinding =
  FloatBinding.emap { d =>
    Extinction.FromMags.getOption(d.toFloat).toRight(s"Invalid Extinction: $d")
  }
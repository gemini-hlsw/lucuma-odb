// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import lucuma.core.util.Enumerated

enum PowerSource(val tag: String) derives Enumerated:
  case Commercial extends PowerSource("COMMERCIAL")
  case Generator  extends PowerSource("GENERATOR")

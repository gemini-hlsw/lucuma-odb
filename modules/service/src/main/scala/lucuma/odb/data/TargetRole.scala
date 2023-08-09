// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

enum TargetRole(val tag: String) derives Enumerated {
  case Science extends TargetRole("science")
  case Guide   extends TargetRole("guide")
}

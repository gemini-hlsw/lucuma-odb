// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model

import lucuma.core.util.Enumerated

enum ComponentLocation(val tag: String) derives Enumerated:
  case Installed extends ComponentLocation("INSTALLED")
  case Floor     extends ComponentLocation("FLOOR")
  case Lab       extends ComponentLocation("LAB")
  case Base      extends ComponentLocation("BASE")
  case Unknown   extends ComponentLocation("UNKNOWN")

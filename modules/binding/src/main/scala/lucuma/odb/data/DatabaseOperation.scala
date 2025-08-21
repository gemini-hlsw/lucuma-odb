// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

enum DatabaseOperation(val tag: String) derives Enumerated:
  case Insert   extends DatabaseOperation("INSERT")
  case Update   extends DatabaseOperation("UPDATE")
  case Delete   extends DatabaseOperation("DELETE")
  case Truncate extends DatabaseOperation("TRUNCATE")
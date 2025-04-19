// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

// The name "TimingWindowEndType" conflicts with the TypeRef for TimingWindowEnd.
enum TimingWindowEndTypeEnum(val tag: String) derives Enumerated:
  case At extends TimingWindowEndTypeEnum("at")
  case After extends TimingWindowEndTypeEnum("after")

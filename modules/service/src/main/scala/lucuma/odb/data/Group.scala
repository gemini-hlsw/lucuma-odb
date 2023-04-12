// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.util.Gid
import lucuma.core.util.WithGid
import lucuma.refined._

object Group extends WithGid('g'.refined)

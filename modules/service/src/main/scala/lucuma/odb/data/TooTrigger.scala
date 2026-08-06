// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.refined.auto.*
import lucuma.core.util.WithGid

/**
 * Target-of-Opportunity trigger, currently just its `Id`.  Defined here in the
 * odb project for convenience while the ToO design settles; move to lucuma-core
 * once it stabilizes.  Uses GID tag 'r' ("request"), which is free in the DB
 * prefix space.
 */
object TooTrigger extends WithGid('r'.refined)

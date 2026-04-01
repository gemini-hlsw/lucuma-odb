// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import lucuma.core.util.Enumerated

/**
 * Visits are created by one client when we slew to a target, and by another
 * when we begin observing.  This should generally be the same visit, despite
 * having been created by different applications at different times.
 * VisitOrigin is used in the implementation of this feature.
 */
enum VisitOrigin(val dbTag: String):
  case Slew    extends VisitOrigin("slew")
  case Observe extends VisitOrigin("observe")

object VisitOrigin:
  given Enumerated[VisitOrigin] =
    Enumerated.from(Slew, Observe).withTag(_.dbTag)
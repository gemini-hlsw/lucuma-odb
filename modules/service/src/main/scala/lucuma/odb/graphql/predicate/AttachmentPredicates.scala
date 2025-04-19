// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import lucuma.core.model.Attachment

class AttachmentPredicates(path: Path) {
  lazy val id        = LeafPredicates[Attachment.Id](path / "id")
  lazy val program   = new ProgramPredicates(path / "program")
}

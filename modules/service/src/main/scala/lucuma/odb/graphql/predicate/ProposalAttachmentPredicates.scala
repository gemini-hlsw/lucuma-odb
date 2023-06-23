// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import edu.gemini.grackle.Path
import lucuma.odb.data.Tag

class ProposalAttachmentPredicates(path: Path) {
  lazy val program   = new ProgramPredicates(path / "program")
  lazy val attachmentType = new LeafPredicates[Tag](path / "attachmentType")
}

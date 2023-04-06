// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Access._
import lucuma.core.model.Attachment
import lucuma.core.model.User
import lucuma.odb.data.Existence

class AttachmentPredicates(path: Path) {
  lazy val id        = LeafPredicates[Attachment.Id](path / "id")
  lazy val program   = new ProgramPredicates(path / "program")
}

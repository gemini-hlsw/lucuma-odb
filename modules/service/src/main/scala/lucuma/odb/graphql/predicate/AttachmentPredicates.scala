// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import cats.syntax.option.*
import cats.syntax.order.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Access
import lucuma.core.model.Attachment
import lucuma.core.model.User

class AttachmentPredicates(path: Path) {
  lazy val id        = LeafPredicates[Attachment.Id](path / "id")
  lazy val program   = new ProgramPredicates(path / "program")

  // Only MOS mask attachments carry an instrument.
  def isMaskFor(instruments: List[Instrument]): Predicate =
    if instruments.isEmpty then False
    else In(path / "maskInstrument", instruments.distinct.map(_.some))

  // The cross-program mask listing is for staff.
  def isListableBy(user: User): Predicate =
    if user.role.access >= Access.Staff then True else False
}

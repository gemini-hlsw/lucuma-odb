// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.util.Enumerated


sealed abstract class EditType(val tag: String) extends Product with Serializable

object EditType {

  case object Created extends EditType("created")
  case object Updated extends EditType("updated")

  implicit val EnumeratedUserType: Enumerated[EditType] =
    Enumerated.from(Created, Updated).withTag(_.tag)

  /**
   * Map `TG_OP` to `EditType`.
   * @see https://www.postgresql.org/docs/9.6/plpgsql-trigger.html
   */
  def fromTgOp(s: String): Option[EditType] =
    s match {
      case "INSERT" => Some(Created)
      case "UPDATE" => Some(Updated)
      case _        => None
    }

}
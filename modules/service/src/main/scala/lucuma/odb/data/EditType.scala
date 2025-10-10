// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

sealed abstract class EditType(val tag: String) extends Product with Serializable

object EditType {

  case object Created    extends EditType("created")
  case object Updated    extends EditType("updated")
  case object HardDelete extends EditType("hard_delete")

  implicit val EnumeratedUserType: Enumerated[EditType] =
    Enumerated.from(Created, Updated, HardDelete).withTag(_.tag)

  /**
   * Map `TG_OP` to `EditType`.
   * @see https://www.postgresql.org/docs/9.6/plpgsql-trigger.html
   */
  def fromTgOp(s: String): Option[EditType] =
    s match {
      case "INSERT" => Some(Created)
      case "UPDATE" => Some(Updated)
      case "DELETE" => Some(HardDelete)
      case _        => None
    }

}

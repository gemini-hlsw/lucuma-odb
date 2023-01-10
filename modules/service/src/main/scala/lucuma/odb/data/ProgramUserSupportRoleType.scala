// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated


sealed abstract class ProgramUserSupportType(val tag: String) extends Product with Serializable

object ProgramUserSupportType {

  case object Staff   extends ProgramUserSupportType("staff")
  case object Partner extends ProgramUserSupportType("partner")

  implicit val EnumeratedProgramUserRole: Enumerated[ProgramUserSupportType] =
    new Enumerated[ProgramUserSupportType] {
      def all: List[ProgramUserSupportType] = List(Staff, Partner)
      def tag(a: ProgramUserSupportType): String = a.tag
    }

}
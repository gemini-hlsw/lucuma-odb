// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated


sealed abstract class ProgramUserRole(val tag: String) extends Product with Serializable

object ProgramUserRole {

  case object Pi       extends ProgramUserRole("pi")
  case object Coi      extends ProgramUserRole("coi")
  case object CoiRO    extends ProgramUserRole("coi_ro")
  case object Support  extends ProgramUserRole("support")

  implicit val EnumeratedProgramUserRole: Enumerated[ProgramUserRole] =
    new Enumerated[ProgramUserRole] {
      def all: List[ProgramUserRole] = List(Pi, Coi, CoiRO, Support)
      def tag(a: ProgramUserRole): String = a.tag
    }

}
package lucuma.odb.data

import lucuma.core.util.Enumerated


sealed abstract class ProgramUserRole(val tag: String) extends Product with Serializable

object ProgramUserRole {

  case object Coi      extends ProgramUserRole("coi")
  case object Observer extends ProgramUserRole("observer")
  case object Support  extends ProgramUserRole("support")

  implicit val EnumeratedProgramUserRole: Enumerated[ProgramUserRole] =
    new Enumerated[ProgramUserRole] {
      def all: List[ProgramUserRole] = List(Coi, Observer, Support)
      def tag(a: ProgramUserRole): String = a.tag
    }

}
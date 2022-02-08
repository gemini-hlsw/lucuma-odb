package lucuma.odb.graphql
import lucuma.odb.graphql.util.Bindings._
import lucuma.core.util.Gid
import lucuma.core.model.Program
import edu.gemini.grackle.Predicate
import eu.timepit.refined.types.string

package object snippet {

  val ProgramIdBinding =
    StringBinding.emap { s =>
      Gid[Program.Id].fromString.getOption(s).toRight(s"'$s' is not a valid ProgramId.")
    }

  val NonEmptyStringBinding =
    StringBinding.emap { s =>
      string.NonEmptyString.unapply(s).toRight("string value must be non-empty.")
    }

  implicit class AndCompanionOps(val self: Predicate.And.type) extends AnyVal {
    def apply(preds: Predicate*): Predicate =
      Predicate.and(preds.toList)
  }

}

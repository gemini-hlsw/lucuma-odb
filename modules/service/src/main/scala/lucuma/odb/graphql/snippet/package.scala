package lucuma.odb.graphql
import lucuma.odb.graphql.util.Bindings._
import lucuma.core.util.Gid
import lucuma.core.model.Program
import edu.gemini.grackle.Predicate
import eu.timepit.refined.types.string
import edu.gemini.grackle.Schema
import scala.io.Source
import lucuma.core.util.Enumerated
import lucuma.odb.data.Existence
import io.circe.Json
import cats.syntax.all._

package object snippet {

  val ProgramIdBinding =
    StringBinding.emap { s =>
      Gid[Program.Id].fromString.getOption(s).toRight(s"'$s' is not a valid ProgramId.")
    }

  def enumeratedBinding[A](implicit ev: Enumerated[A]) =
    TypedEnumBinding.map(b => Json.fromString(b.name)).emap { j =>
      ev.decodeJson(j).leftMap(_.message)
    }

  val ExistenceBinding =
    enumeratedBinding[Existence]

  val NonEmptyStringBinding =
    StringBinding.emap { s =>
      string.NonEmptyString.unapply(s).toRight("string value must be non-empty.")
    }

  implicit class AndCompanionOps(val self: Predicate.And.type) extends AnyVal {
    def apply(preds: Predicate*): Predicate =
      Predicate.and(preds.toList)
  }

  // Loads a GraphQL file with the same simple classname as the given object, minus the trailing $
  def unsafeLoadSchema(snippetObject: AnyRef): Schema = {
    val clazz    = snippetObject.getClass
    val fileName = s"${clazz.getSimpleName().dropRight(1)}.graphql"
    val stream = clazz.getResourceAsStream(fileName)
    val src  = Source.fromInputStream(stream, "UTF-8")
    try Schema(src.getLines().mkString("\n")).getOrElse(sys.error(s"Invalid schema: $fileName"))
    finally src.close()
  }

}

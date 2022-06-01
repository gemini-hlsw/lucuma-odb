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
import lucuma.odb.data.Tag
import lucuma.core.model.User
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.data.ObsStatus
import lucuma.odb.data.ObsActiveStatus
import lucuma.core.math.Epoch
import scala.util.control.NonFatal
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle

package object snippet {

  val ProgramIdBinding =
    StringBinding.emap { s =>
      Gid[Program.Id].fromString.getOption(s).toRight(s"'$s' is not a valid Program.Id")
    }

  val UserIdBinding =
    StringBinding.emap { s =>
      Gid[User.Id].fromString.getOption(s).toRight(s"'$s' is not a valid User.Id")
    }

  def enumeratedBinding[A](implicit ev: Enumerated[A]) =
    TypedEnumBinding.map(b => Json.fromString(b.name)).emap { j =>
      ev.decodeJson(j).leftMap(_.message)
    }

  val ExistenceBinding =
    enumeratedBinding[Existence]

  val ProgramUserRoleBinding =
    enumeratedBinding[ProgramUserRole]

  val ProgramUserSupportRoleTypeBinding =
    enumeratedBinding[ProgramUserSupportType]

  val NonEmptyStringBinding =
    StringBinding.emap { s =>
      string.NonEmptyString.unapply(s).toRight("string value must be non-empty.")
    }

  val TagBinding =
    TypedEnumBinding.map(v => Tag(v.name.toLowerCase))

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

  val ObsStatusBinding: Matcher[ObsStatus] =
    enumeratedBinding

  val ObsActiveStatusBinding: Matcher[ObsActiveStatus] =
    enumeratedBinding

  val EpochBinding: Matcher[Epoch] =
    StringBinding.emap { s =>
      Epoch.fromString.getOption(s).toRight(s"Invalid epoch: $s")
    }

  val LongBinding: Matcher[Long] =
    StringBinding.emap { s =>
      try Right(s.toLong)
      catch { case NonFatal(e) => Left(s"Invalid Long: $s: ${e.getMessage}") }
    }

  val BigDecimalBinding: Matcher[BigDecimal] =
    StringBinding.emap { s =>
      try Right(BigDecimal(s))
      catch { case NonFatal(e) => Left(s"Invalid BigDecimal: $s: ${e.getMessage}") }
    }

  val DmsBinding: Matcher[Angle] =
    StringBinding.emap { s =>
      Angle.fromStringDMS.getOption(s).toRight(s"Invalid angle: $s")
    }

}

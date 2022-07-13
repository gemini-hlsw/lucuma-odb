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
import edu.gemini.grackle.Cursor
import org.tpolecat.typename._
import scala.reflect.ClassTag
import edu.gemini.grackle.Result
import skunk.Codec
import edu.gemini.grackle.sql.FailedJoin
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits

package object snippet {

    implicit class EnvOps(self: Cursor.Env) {
      def getR[A: ClassTag: TypeName](name: String): Result[A] =
        self.get[A](name) match {
          case None        => Result.failure(s"Key '$name' of type ${typeName[A]} was not found in $self")
          case Some(value) => Result(value)
        }
    }

  implicit class ResultCompanionOps(self: Result.type) {
    def fromOption[A](oa: Option[A], ifNone: => String): Result[A] =
      oa match {
        case Some(a) => Result(a)
        case None    => Result.failure(ifNone)
      }

    def warning[A](warning: String, value: A): Result[A] =
      Result.failure[A](warning).putRight(value)

  }

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
    try Schema(src.getLines().mkString("\n")).toEither.fold(x => sys.error(s"Invalid schema: $fileName: ${x.toList.mkString(", ")}"), identity)
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

  implicit class CodecOps[A](self: Codec[A]) {
    def embedded: Codec[Any] =
      self.opt.imap(_.getOrElse(FailedJoin))(x => Some(x.asInstanceOf[A])) // whee
  }

  val BandBinding: Matcher[Band] =
    enumeratedBinding[Band]

  val x: Matcher[BrightnessUnits.Integrated] =
    ???

}

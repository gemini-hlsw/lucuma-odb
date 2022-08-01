// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Result
import edu.gemini.grackle.sql.FailedJoin
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import lucuma.core.enums.Band
import lucuma.core.math.Angle
import lucuma.core.math.Epoch
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.odb.data.Existence
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.data.Tag
import lucuma.odb.graphql.util.Bindings._
import org.tpolecat.typename._
import skunk.Codec

import scala.reflect.ClassTag
import scala.util.control.NonFatal

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

    def fromEither[A](ea: Either[String, A]): Result[A] =
      ea.fold(Result.failure(_), Result.apply)

    def warning[A](warning: String, value: A): Result[A] =
      Result.failure[A](warning).putRight(value)

  }

  def gidBinding[A: Gid](name: String): Matcher[A] =
    StringBinding.emap { s =>
      Gid[A].fromString.getOption(s).toRight(s"'$s' is not a valid $name id")
    }

  val ProgramIdBinding: Matcher[Program.Id] =
    gidBinding[Program.Id]("program")

  val ObservationIdBinding: Matcher[Observation.Id] =
    gidBinding[Observation.Id]("observation")

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
      NonEmptyString.unapply(s).toRight("string value must be non-empty.")
    }

  val NonNegIntBinding =
    IntBinding.emap(NonNegInt.from)

  val TagBinding =
    TypedEnumBinding.map(v => Tag(v.name.toLowerCase))

  implicit class AndCompanionOps(val self: Predicate.And.type) extends AnyVal {
    def all(preds: Predicate*): Predicate =
      Predicate.and(preds.toList)
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

  val PosBigDecimalBinding: Matcher[PosBigDecimal] =
    BigDecimalBinding.emap { s =>
      PosBigDecimal.from(s).leftMap(m => s"Invalid PosBigDecimal: $s: $m")
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

}

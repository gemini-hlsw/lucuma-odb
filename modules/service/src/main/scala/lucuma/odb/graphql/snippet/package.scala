// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
import cats.syntax.all._
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Schema
import eu.timepit.refined.types.string
import io.circe.Json
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

import scala.io.Source

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

}

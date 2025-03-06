// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.ProgramNote
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*


object ProgramNotePropertiesInput:

  case class Create(
    title:     NonEmptyString,
    text:      Option[NonEmptyString],
    isPrivate: Boolean,
    before:    Option[ProgramNote.Id],
    existence: Existence
  )

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          NonEmptyStringBinding("title", rTitle),
          NonEmptyStringBinding.Option("text", rText),
          BooleanBinding.Option("isPrivate", rIsPrivate),
          ProgramNoteIdBinding.Option("before", rBefore),
          ExistenceBinding.Option("existence", rExistence)
        ) =>
          (rTitle, rText, rIsPrivate, rBefore, rExistence).parMapN: (title, text, isPrivate, before, existence) =>
            Create(
              title,
              text,
              isPrivate.getOrElse(false),
              before,
              existence.getOrElse(Existence.Present)
            )

  case class Edit(
    title:     Option[NonEmptyString],
    text:      Nullable[NonEmptyString],
    isPrivate: Option[Boolean],
    before:    Nullable[ProgramNote.Id],
    existence: Option[Existence]
  )

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          NonEmptyStringBinding.Option("title", rTitle),
          NonEmptyStringBinding.Nullable("text", rText),
          BooleanBinding.Option("isPrivate", rIsPrivate),
          ProgramNoteIdBinding.Nullable("before", rBefore),
          ExistenceBinding.Option("existence", rExistence)
        ) =>
          (rTitle, rText, rIsPrivate, rBefore, rExistence).parMapN(Edit.apply)
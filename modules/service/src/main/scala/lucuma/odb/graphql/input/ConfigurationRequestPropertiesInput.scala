// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.* 

object ConfigurationRequestPropertiesInput {
  private val StatusBinding = enumeratedBinding[ConfigurationRequestStatus]

  case class Create(
    justification: Option[NonEmptyString]
  )

  object Create:
    val Empty = Create(None)
    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          StatusBinding.Option("status", rStatus),
          NonEmptyStringBinding.Option("justification", rJust),
          NonEmptyStringBinding.Option("feedback", rFeedback)
        ) =>
          (rStatus, rJust, rFeedback).tupled.flatMap:
            case (None, just, None) => Result(Create(just))
            case (Some(_), _, _)    =>
              OdbError.InvalidArgument(Some("Status may not be specified on creation")).asFailure
            case (_, _, Some(_))    =>
              OdbError.InvalidArgument(Some("Feedback may not be specified on creation")).asFailure

  case class Update(
    status: Option[ConfigurationRequestStatus],
    justification: Nullable[NonEmptyString],
    feedback: Nullable[NonEmptyString]
  )

  object Update:
    val Binding: Matcher[Update] =
      ObjectFieldsBinding.rmap:
        case List(
          StatusBinding.Option("status", rStatus),
          NonEmptyStringBinding.Nullable("justification", rJust),
          NonEmptyStringBinding.Nullable("feedback", rFeedback)
        ) =>
          (rStatus, rJust, rFeedback).parMapN(Update.apply)

}

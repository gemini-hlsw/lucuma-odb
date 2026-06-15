// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.Ior
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.syntax.*
import lucuma.core.model.Semester
import lucuma.core.syntax.string.*
import lucuma.core.util.DateInterval
import lucuma.core.util.Timestamp
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.data.Observatory
import lucuma.odb.graphql.binding.*

import java.time.LocalDate

object CallForProposalsPropertiesInput:

  case class Create(
    semester:   Semester,
    title:      Option[NonEmptyString],
    active:     DateInterval,
    deadline:   Option[Timestamp],
    partners:   Option[List[CallForProposalsPartnerInput]],
    existence:  Existence,
    properties: Create.Properties
  ):
    def observatory: Observatory =
      properties.observatory

  object Create:

    enum Properties:
      case Gemini(props: GeminiCallPropertiesInput.Create)
      case Keck(props: KeckCallPropertiesInput.Create)
      case Subaru(props: SubaruCallPropertiesInput.Create)

      def observatory: Observatory =
        this match
          case Gemini(_) => Observatory.Gemini
          case Keck(_)   => Observatory.Keck
          case Subaru(_) => Observatory.Subaru

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          SemesterBinding("semester", rSemester),
          NonEmptyStringBinding.Option("title", rTitle),
          DateBinding("activeStart", rActiveStart),
          DateBinding("activeEnd",   rActiveEnd),
          CallForProposalsPartnerInput.Binding.List.Option("partners", rPartners),
          TimestampBinding.Option("submissionDeadlineDefault", rDeadline),
          ExistenceBinding.Option("existence", rExistence),
          GeminiCallPropertiesInput.Create.Binding.Option("gemini", rGemini),
          KeckCallPropertiesInput.Create.Binding.Option("keck",     rKeck),
          SubaruCallPropertiesInput.Create.Binding.Option("subaru", rSubaru)
        ) =>
          val rActive    = date.validateInputInterval("activeStart", "activeEnd", rActiveStart, rActiveEnd)
          val rPartnersʹ = mapDedup("partners", rPartners)(_.partner, _.tag.toScreamingSnakeCase)

          val rProperties: Result[DateInterval => Properties] =
            (rGemini, rKeck, rSubaru).parTupled.flatMap:
              case (Some(g), None, None) => ((active: DateInterval) => Properties.Gemini(g(active))).success
              case (None, Some(k), None) => ((active: DateInterval) => Properties.Keck(k(active))).success
              case (None, None, Some(s)) => ((active: DateInterval) => Properties.Subaru(s(active))).success
              case _                     => Matcher.validationFailure("Exactly one of 'gemini', 'keck' or 'subaru' must be provided.")

          (
            rSemester,
            rTitle,
            rActive,
            rDeadline,
            rPartnersʹ,
            rExistence.map(_.getOrElse(Existence.Present)),
            rProperties
          ).parMapN: (semester, title, active, deadline, partners, exist, properties) =>
            Create(
              semester,
              title,
              active,
              deadline,
              partners,
              exist,
              properties(active)
            )

  case class Edit(
    semester:   Option[Semester],
    title:      Nullable[NonEmptyString],
    active:     Option[Ior[LocalDate, LocalDate]],
    deadline:   Nullable[Timestamp],
    partners:   Nullable[List[CallForProposalsPartnerInput]],
    existence:  Option[Existence],
    properties: Option[Edit.Properties]
  ):
    def observatory: Option[Observatory] =
      properties.map(_.observatory)

  object Edit:

    enum Properties:
      case Gemini(props: GeminiCallPropertiesInput.Edit)
      case Keck(props: KeckCallPropertiesInput.Edit)
      case Subaru(props: SubaruCallPropertiesInput.Edit)

      def observatory: Observatory =
        this match
          case Gemini(_) => Observatory.Gemini
          case Keck(_)   => Observatory.Keck
          case Subaru(_) => Observatory.Subaru

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          SemesterBinding.NonNullable("semester", rSemester),
          NonEmptyStringBinding.Nullable("title", rTitle),
          DateBinding.NonNullable("activeStart", rActiveStart),
          DateBinding.NonNullable("activeEnd",   rActiveEnd),
          CallForProposalsPartnerInput.Binding.List.Nullable("partners", rPartners),
          TimestampBinding.Nullable("submissionDeadlineDefault", rDeadline),
          ExistenceBinding.NonNullable("existence", rExistence),
          GeminiCallPropertiesInput.Edit.Binding.Option("gemini", rGemini),
          KeckCallPropertiesInput.Edit.Binding.Option("keck",     rKeck),
          SubaruCallPropertiesInput.Edit.Binding.Option("subaru", rSubaru)
        ) =>
          val rActive    = date.validateOptionalInputInterval("activeStart", "activeEnd", rActiveStart, rActiveEnd)
          val rPartnersʹ = mapDedup("partners", rPartners)(_.partner, _.tag.toScreamingSnakeCase)

          val rProperties: Result[Option[Properties]] =
            (rGemini, rKeck, rSubaru).parTupled.flatMap:
              case (Some(g), None, None) => Properties.Gemini(g).some.success
              case (None, Some(k), None) => Properties.Keck(k).some.success
              case (None, None, Some(s)) => Properties.Subaru(s).some.success
              case (None, None, None)    => none.success
              case _                     => Matcher.validationFailure("Only one of 'gemini', 'keck' or 'subaru' may be provided.")

          (
            rSemester,
            rTitle,
            rActive,
            rDeadline,
            rPartnersʹ,
            rExistence,
            rProperties
          ).parMapN(Edit.apply)
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.Traverse
import cats.data.Ior
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.Instrument
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Semester
import lucuma.core.syntax.string.*
import lucuma.core.util.DateInterval
import lucuma.core.util.Timestamp
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

import java.time.LocalDate

object CallForProposalsPropertiesInput {

  case class Create(
    cfpType:     CallForProposalsType,
    semester:    Semester,
    title:       Option[NonEmptyString],
    gnRaLimit:   (RightAscension, RightAscension),
    gnDecLimit:  (Declination, Declination),
    gsRaLimit:   (RightAscension, RightAscension),
    gsDecLimit:  (Declination, Declination),
    active:      DateInterval,
    deadline:    Option[Timestamp],
    partners:    Option[List[CallForProposalsPartnerInput]],
    instruments: List[Instrument],
    proprietary: Option[NonNegInt],
    existence:   Existence
  )

  object Create {

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsTypeBinding("type", rType),
          SemesterBinding("semester", rSemester),
          NonEmptyStringBinding.Option("title", rTitle),
          SiteCoordinateLimitsInput.Create.Binding.Option("coordinateLimits", rLimits),
          DateBinding("activeStart", rActiveStart),
          DateBinding("activeEnd",   rActiveEnd),
          TimestampBinding.Option("submissionDeadlineDefault", rDeadline),
          CallForProposalsPartnerInput.Binding.List.Option("partners", rPartners),
          InstrumentBinding.List.Option("instruments", rInstruments),
          NonNegIntBinding.Option("proprietaryMonths", rProprietary),
          ExistenceBinding.Option("existence", rExistence)
        ) => {
          val rActive       = date.validateInputInterval("activeStart", "activeEnd", rActiveStart, rActiveEnd)
          val rPartnersʹ    = dedup("partners",    rPartners)(_.partner, _.tag.toScreamingSnakeCase)
          val rInstrumentsʹ = dedup("instruments", rInstruments)(identity, _.tag.toScreamingSnakeCase).map(_.toList.flatten)
          (
            rType,
            rSemester,
            rTitle,
            rLimits,
            rActive,
            rDeadline,
            rPartnersʹ,
            rInstrumentsʹ,
            rProprietary,
            rExistence.map(_.getOrElse(Existence.Present))
          ).parMapN { (cfpType, semester, title, limits, active, deadline, partners, instruments, proprietary, exist) =>
            val coords = limits.fold(SiteCoordinateLimitsInput.Create.default(active))(f => f(active))
            Create(
              cfpType,
              semester,
              title,
              (coords.north.raStart, coords.north.raEnd),
              (coords.north.decStart, coords.north.decEnd),
              (coords.south.raStart, coords.south.raEnd),
              (coords.south.decStart, coords.south.decEnd),
              active,
              deadline,
              partners,
              instruments,
              proprietary,
              exist
            )
          }
        }
      }

  }

  case class Edit(
    cfpType:     Option[CallForProposalsType],
    semester:    Option[Semester],
    title:       Nullable[NonEmptyString],
    gnRaLimit:   (Option[RightAscension], Option[RightAscension]),
    gnDecLimit:  (Option[Declination], Option[Declination]),
    gsRaLimit:   (Option[RightAscension], Option[RightAscension]),
    gsDecLimit:  (Option[Declination], Option[Declination]),
    active:      Option[Ior[LocalDate, LocalDate]],
    deadline:    Nullable[Timestamp],
    partners:    Nullable[List[CallForProposalsPartnerInput]],
    instruments: Nullable[List[Instrument]],
    proprietary: Option[NonNegInt],
    existence:   Option[Existence]
  )

  object Edit {

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsTypeBinding.NonNullable("type", rType),
          SemesterBinding.NonNullable("semester", rSemester),
          NonEmptyStringBinding.Nullable("title", rTitle),
          SiteCoordinateLimitsInput.Edit.Binding.Option("coordinateLimits", rLimits),
          DateBinding.NonNullable("activeStart", rActiveStart),
          DateBinding.NonNullable("activeEnd",   rActiveEnd),
          TimestampBinding.Nullable("submissionDeadlineDefault", rDeadline),
          CallForProposalsPartnerInput.Binding.List.Nullable("partners", rPartners),
          InstrumentBinding.List.Nullable("instruments", rInstruments),
          NonNegIntBinding.Option("proprietaryMonths", rProprietary),
          ExistenceBinding.NonNullable("existence", rExistence)
        ) => {
          val rLimNorth = rLimits.map(_.flatMap(_.north))
          val rLimSouth = rLimits.map(_.flatMap(_.south))

          val rActive       = date.validateOptionalInputInterval("activeStart", "activeEnd", rActiveStart, rActiveEnd)
          val rPartnersʹ    = dedup("partners",    rPartners)(_.partner, _.tag.toScreamingSnakeCase)
          val rInstrumentsʹ = dedup("instruments", rInstruments)(identity, _.tag.toScreamingSnakeCase)
          (
            rType,
            rSemester,
            rTitle,
            rLimNorth.map(lim => (lim.flatMap(_.raStart), lim.flatMap(_.raEnd))),
            rLimNorth.map(lim => (lim.flatMap(_.decStart), lim.flatMap(_.decEnd))),
            rLimSouth.map(lim => (lim.flatMap(_.raStart), lim.flatMap(_.raEnd))),
            rLimSouth.map(lim => (lim.flatMap(_.decStart), lim.flatMap(_.decEnd))),
            rActive,
            rDeadline,
            rPartnersʹ,
            rInstrumentsʹ,
            rProprietary,
            rExistence
          ).parMapN(Edit.apply)
        }
      }

  }

  private def dedup[F[_]: Traverse, A, B](
    name: String,
    in:   Result[F[List[A]]]
  )(
    f: A => B,
    p: B => String
  ): Result[F[List[A]]] =
    in.flatMap { fas =>
      fas.traverse { as =>
        val bs   = as.map(f)
        val dups = bs.diff(bs.distinct)
        if (dups.isEmpty) as.success
        else Matcher.validationFailure(s"duplicate '$name' specified: ${dups.map(p).mkString(", ")}")
      }
   }
}

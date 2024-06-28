// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.Traverse
import cats.data.Ior
import cats.syntax.all.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.Instrument
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Semester
import lucuma.core.syntax.string.*
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object CallForProposalsPropertiesInput {

  // We default RA and Dec from the LST of the active period start/end.  Our
  // code for computing the LST only works until 2100 apparently.
  private def validateTime(name: String, rTime: Result[Timestamp]): Result[Timestamp] =
    rTime.flatMap { time =>
      val year = time.toLocalDateTime.getYear  // year in UTC timezone
      if (year < 2100) time.success
      else Matcher.validationFailure(s"'$name' time must be before the year 2100 UTC")
    }

  case class Create(
    cfpType:     CallForProposalsType,
    semester:    Semester,
    gnRaLimit:   (RightAscension, RightAscension),
    gnDecLimit:  (Declination, Declination),
    gsRaLimit:   (RightAscension, RightAscension),
    gsDecLimit:  (Declination, Declination),
    active:      TimestampInterval,
    deadline:    Option[Timestamp],
    partners:    Option[List[CallForProposalsPartnerInput]],
    instruments: List[Instrument],
    existence:   Existence
  )

  object Create {

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsTypeBinding("type", rType),
          SemesterBinding("semester", rSemester),
          SiteCoordinateLimitsInput.Create.Binding.Option("coordinateLimits", rLimits),
          TimestampBinding("activeStart", rActiveStart),
          TimestampBinding("activeEnd",   rActiveEnd),
          TimestampBinding.Option("submissionDeadlineDefault", rDeadline),
          CallForProposalsPartnerInput.Binding.List.Option("partners", rPartners),
          InstrumentBinding.List.Option("instruments", rInstruments),
          ExistenceBinding.Option("existence", rExistence)
        ) => {
          // Check that active start comes before end.
          val rValidStart = validateTime("activeStart", rActiveStart)
          val rValidEnd   = validateTime("activeEnd", rActiveEnd)
          val rActive = (rValidStart, rValidEnd).parTupled.flatMap { (start, end) =>
            Result.fromOption(
              Option.when(start < end)(TimestampInterval.between(start, end)),
              Matcher.validationProblem("activeStart must come before activeEnd")
            )
          }

          val rPartnersʹ    = dedup("partners",    rPartners)(_.partner, _.tag.toScreamingSnakeCase)
          val rInstrumentsʹ = dedup("instruments", rInstruments)(identity, _.tag.toScreamingSnakeCase).map(_.toList.flatten)
          (
            rType,
            rSemester,
            rLimits,
            rActive,
            rDeadline,
            rPartnersʹ,
            rInstrumentsʹ,
            rExistence.map(_.getOrElse(Existence.Present))
          ).parMapN { (cfpType, semester, limits, active, deadline, partners, instruments, exist) =>
            val coords = limits.fold(SiteCoordinateLimitsInput.Create.default(active))(f => f(active))
            Create(
              cfpType,
              semester,
              (coords.north.raStart, coords.north.raEnd),
              (coords.north.decStart, coords.north.decEnd),
              (coords.south.raStart, coords.south.raEnd),
              (coords.south.decStart, coords.south.decEnd),
              active,
              deadline,
              partners,
              instruments,
              exist
            )
          }
        }
      }

  }

  case class Edit(
    cfpType:     Option[CallForProposalsType],
    semester:    Option[Semester],
    gnRaLimit:   (Option[RightAscension], Option[RightAscension]),
    gnDecLimit:  (Option[Declination], Option[Declination]),
    gsRaLimit:   (Option[RightAscension], Option[RightAscension]),
    gsDecLimit:  (Option[Declination], Option[Declination]),
    active:      Option[Ior[Timestamp, Timestamp]],
    deadline:    Nullable[Timestamp],
    partners:    Nullable[List[CallForProposalsPartnerInput]],
    instruments: Nullable[List[Instrument]],
    existence:   Option[Existence]
  )

  object Edit {

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsTypeBinding.NonNullable("type", rType),
          SemesterBinding.NonNullable("semester", rSemester),
          SiteCoordinateLimitsInput.Edit.Binding.Option("coordinateLimits", rLimits),
          TimestampBinding.NonNullable("activeStart", rActiveStart),
          TimestampBinding.NonNullable("activeEnd",   rActiveEnd),
          TimestampBinding.Nullable("submissionDeadlineDefault", rDeadline),
          CallForProposalsPartnerInput.Binding.List.Nullable("partners", rPartners),
          InstrumentBinding.List.Nullable("instruments", rInstruments),
          ExistenceBinding.NonNullable("existence", rExistence)
        ) => {
          val rLimNorth = rLimits.map(_.flatMap(_.north))
          val rLimSouth = rLimits.map(_.flatMap(_.south))

          // If both start and end are specified, they should be in order.
          val rActive = (rActiveStart, rActiveEnd).parTupled.flatMap {
            case (Some(start), Some(end)) if start > end =>
              Matcher.validationFailure("activeStart must come before activeEnd")
            case (s, e)                                  =>
              Result(Ior.fromOptions(s, e))
          }
          val rPartnersʹ    = dedup("partners",    rPartners)(_.partner, _.tag.toScreamingSnakeCase)
          val rInstrumentsʹ = dedup("instruments", rInstruments)(identity, _.tag.toScreamingSnakeCase)
          (
            rType,
            rSemester,
            rLimNorth.map(lim => (lim.flatMap(_.raStart), lim.flatMap(_.raEnd))),
            rLimNorth.map(lim => (lim.flatMap(_.decStart), lim.flatMap(_.decEnd))),
            rLimSouth.map(lim => (lim.flatMap(_.raStart), lim.flatMap(_.raEnd))),
            rLimSouth.map(lim => (lim.flatMap(_.decStart), lim.flatMap(_.decEnd))),
            rActive,
            rDeadline,
            rPartnersʹ,
            rInstrumentsʹ,
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

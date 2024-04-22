// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.Ior
import cats.syntax.all.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Semester
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.CallForProposalsType
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object CallForProposalsPropertiesInput {

  case class Create(
    cfpType:     CallForProposalsType,
    semester:    Semester,
    raLimit:     Option[(RightAscension, RightAscension)],
    decLimit:    Option[(Declination, Declination)],
    active:      TimestampInterval,
    partners:    List[CallForProposalsPartnerInput],
    instruments: List[Instrument],
    existence:   Existence
  )

  object Create {

    private def bothOrNeither[A](
      ra: Result[Option[A]],
      rb: Result[Option[A]],
      na: String,
      nb: String
    ): Result[Option[(A, A)]] =
      (ra, rb).parFlatMapN {
        case (Some(a), Some(b)) => Some((a, b)).success
        case (None, None)       => None.success
        case _                  => Result.failure(s"Supply both $na and $nb or neither")
      }

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsTypeBinding("type", rType),
          SemesterBinding("semester", rSemester),
          RightAscensionInput.Binding.Option("raLimitStart", rRaStart),
          RightAscensionInput.Binding.Option("raLimitEnd",   rRaEnd),
          DeclinationInput.Binding.Option("decLimitStart",   rDecStart),
          DeclinationInput.Binding.Option("decLimitEnd",     rDecEnd),
          TimestampBinding("activeStart", rActiveStart),
          TimestampBinding("activeEnd",   rActiveEnd),
          CallForProposalsPartnerInput.Binding.List.Option("partners", rPartners),
          InstrumentBinding.List.Option("instruments", rInstruments),
          ExistenceBinding.Option("existence", rExistence)
        ) => {
          // Check that both (or neither) limits are supplied.
          val rRaLimit  = bothOrNeither(rRaStart,  rRaEnd,  "raLimitStart",  "raLimitEnd")
          val rDecLimit = bothOrNeither(rDecStart, rDecEnd, "decLimitStart", "decLimitEnd")
          // Check that active start comes before end.
          val rActive = (rActiveStart, rActiveEnd).parTupled.flatMap { (start, end) =>
            Result.fromOption(
              Option.when(start <= end)(TimestampInterval.between(start, end)),
              Matcher.validationProblem("activeStart must come before activeEnd")
            )
          }
          (
            rType,
            rSemester,
            rRaLimit,
            rDecLimit,
            rActive,
            rPartners.map(_.toList.flatten),
            rInstruments.map(_.toList.flatten),
            rExistence.map(_.getOrElse(Existence.Present))
          ).parMapN(Create.apply)
        }
      }

  }

  case class Edit(
    cfpType:     Option[CallForProposalsType],
    semester:    Option[Semester],
    active:      Option[Ior[Timestamp, Timestamp]],
    instruments: Nullable[List[Instrument]],
    existence:   Option[Existence]
  )

  object Edit {

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsTypeBinding.NonNullable("type", rType),
          SemesterBinding.NonNullable("semester", rSemester),
          RightAscensionInput.Binding.Nullable("raLimitStart", rRaStart),
          RightAscensionInput.Binding.Nullable("raLimitEnd",   rRaEnd),
          DeclinationInput.Binding.Nullable("decLimitStart",   rDecStart),
          DeclinationInput.Binding.Nullable("decLimitEnd",     rDecEnd),
          TimestampBinding.NonNullable("activeStart", rActiveStart),
          TimestampBinding.NonNullable("activeEnd",   rActiveEnd),
          CallForProposalsPartnerInput.Binding.List.Nullable("partners", rPartners),
          InstrumentBinding.List.Nullable("instruments", rInstruments),
          ExistenceBinding.NonNullable("existence", rExistence)
        ) => {
          // If both start and end are specified, they should be in order.
          val rActive = (rActiveStart, rActiveEnd).parTupled.flatMap {
            case (Some(start), Some(end)) if start > end =>
              Matcher.validationFailure("activeStart must come before activeEnd")
            case (s, e)                                  =>
              Result(Ior.fromOptions(s, e))
          }

          (
            rType,
            rSemester,
            rActive,
            rInstruments,
            rExistence
          ).parMapN(Edit.apply)
        }
      }

  }

}

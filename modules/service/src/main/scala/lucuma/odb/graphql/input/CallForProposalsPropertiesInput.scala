// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.order.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.Instrument
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Semester
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.CallForProposalsStatus
import lucuma.odb.data.CallForProposalsType
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding.*

object CallForProposalsPropertiesInput {

  case class Create(
    status:        CallForProposalsStatus,
    cfpType:       CallForProposalsType,
    semester:      Semester,
    raLimitStart:  Option[RightAscension],
    raLimitEnd:    Option[RightAscension],
    decLimitStart: Option[Declination],
    decLimitEnd:   Option[Declination],
    active:        TimestampInterval,
    partners:      List[CallForProposalsPartnerInput],
    include:       List[Instrument],
    existence:     Existence
  )

  object Create {

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsStatusBinding.Option("status", rStatus),
          CallForProposalsTypeBinding("type", rType),
          SemesterBinding("semester", rSemester),
          RightAscensionInput.Binding.Option("raLimitStart", rRaStart),
          RightAscensionInput.Binding.Option("raLimitEnd",   rRaEnd),
          DeclinationInput.Binding.Option("decLimitStart",   rDecStart),
          DeclinationInput.Binding.Option("decLimitEnd",     rDecEnd),
          TimestampBinding("activeStart", rActiveStart),
          TimestampBinding("activeEnd",   rActiveEnd),
          CallForProposalsPartnerInput.Binding.List.Option("partners", rPartners),
          InstrumentBinding.List.Option("includeInstruments", rInclude),
          ExistenceBinding.Option("existence", rExistence)
        ) => {
          val rActive = (rActiveStart, rActiveEnd).parTupled.flatMap { (start, end) =>
            Result.fromOption(
              Option.when(start <= end)(TimestampInterval.between(start, end)),
              Matcher.validationProblem("activeStart must be before activeEnd")
            )
          }
          (
            rStatus.map(_.getOrElse(CallForProposalsStatus.Closed)),
            rType,
            rSemester,
            rRaStart,
            rRaEnd,
            rDecStart,
            rDecEnd,
            rActive,
            rPartners.map(_.toList.flatten),
            rInclude.map(_.toList.flatten),
            rExistence.map(_.getOrElse(Existence.Present))
          ).parMapN(Create.apply)
        }
      }

  }

}

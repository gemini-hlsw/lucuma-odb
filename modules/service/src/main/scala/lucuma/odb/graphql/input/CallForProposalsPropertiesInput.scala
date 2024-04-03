// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Semester
import lucuma.core.util.Timestamp
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
    activeStart:   Timestamp,
    activeEnd:     Timestamp,
    partners:      List[CallForProposalsPartnerInput],
    include:       List[Instrument],
    exclude:       List[Instrument],
    existence:     Option[Existence]
  )

  object Create {

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CallForProposalsStatusBinding("status", rStatus),
          CallForProposalsTypeBinding("type", rType),
          SemesterBinding("semester", rSemester),
          RightAscensionInput.Binding.Option("raLimitStart", rRaStart),
          RightAscensionInput.Binding.Option("raLimitEnd",   rRaEnd),
          DeclinationInput.Binding.Option("decLimitStart",   rDecStart),
          DeclinationInput.Binding.Option("decLimitEnd",     rDecEnd),
          TimestampBinding("activeStart", rActiveStart),
          TimestampBinding("activeEnd",   rActiveEnd),
          CallForProposalsPartnerInput.Binding.List("partners", rPartners),
          InstrumentBinding.List("includeInstruments", rInclude),
          InstrumentBinding.List("excludeInstruments", rExclude),
          ExistenceBinding.Option("existence", rExistence)
        ) => (
          rStatus,
          rType,
          rSemester,
          rRaStart,
          rRaEnd,
          rDecStart,
          rDecEnd,
          rActiveStart,
          rActiveEnd,
          rPartners,
          rInclude,
          rExclude,
          rExistence
        ).parMapN(Create.apply)
      }

  }

}

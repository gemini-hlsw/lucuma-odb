// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Semester
import lucuma.odb.graphql.binding.InstrumentBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.SemesterBinding

case class ProgramReferencePropertiesMonitoringInput(
  semester:   Semester,
  instrument: Instrument
)

object ProgramReferencePropertiesMonitoringInput {

  val Binding: Matcher[ProgramReferencePropertiesMonitoringInput] =
    ObjectFieldsBinding.rmap {
      case List(
        SemesterBinding("semester", rSemester),
        InstrumentBinding("instrument", rInstrument)
      ) => (rSemester, rInstrument).parMapN(apply)
    }

}
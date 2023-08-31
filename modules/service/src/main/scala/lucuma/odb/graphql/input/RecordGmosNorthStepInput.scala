// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.odb.graphql.binding.*

case class RecordGmosNorthStepInput(
  atomId:     Atom.Id,
  instrument: GmosNorth,
  step:       StepConfig
)

object RecordGmosNorthStepInput {

  val Binding: Matcher[RecordGmosNorthStepInput] =
    ObjectFieldsBinding.rmap {
      case List(
        AtomIdBinding("atomId", rAtomId),
        GmosNorthDynamicInput.Binding("instrument", rInstrument),
        StepConfigInput.Binding("stepConfig", rStepConfig)
      ) => (rAtomId, rInstrument, rStepConfig).parMapN { (atomId, instrument, step) =>
        RecordGmosNorthStepInput(atomId, instrument, step)
      }
    }

}

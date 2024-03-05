// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.ObserveClass
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.odb.graphql.binding.*

case class RecordGmosStepInput[A](
  atomId:       Atom.Id,
  instrument:   A,
  step:         StepConfig,
  observeClass: ObserveClass
)

object RecordGmosStepInput {

  private def binding[A](
    instrumentName:    String,
    instrumentMatcher: Matcher[A]
  ): Matcher[RecordGmosStepInput[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        AtomIdBinding("atomId", rAtomId),
        instrumentMatcher(`instrumentName`, rInstrument),
        StepConfigInput.Binding("stepConfig", rStepConfig),
        ObserveClassBinding("observeClass", rObserveClass)
      ) => (rAtomId, rInstrument, rStepConfig, rObserveClass).parMapN { (atomId, instrument, step, oclass) =>
        RecordGmosStepInput(atomId, instrument, step, oclass)
      }
    }

  val GmosNorthBinding: Matcher[RecordGmosStepInput[GmosNorth]] =
    binding("gmosNorth", GmosNorthDynamicInput.Binding)

  val GmosSouthBinding: Matcher[RecordGmosStepInput[GmosSouth]] =
    binding("gmosSouth", GmosSouthDynamicInput.Binding)

}

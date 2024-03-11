// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.kernel.laws.discipline.*
import lucuma.core.optics.laws.discipline.*
import lucuma.odb.data.arb.ArbObservationReference

final class ObservationReferenceSuite extends munit.DisciplineSuite {

  import ArbObservationReference.given
  import ArbObservationReference.observationReferenceStrings

  checkAll("ObservationReference", OrderTests[ObservationReference].order)
  checkAll("ObservationReference", FormatTests(ObservationReference.fromString).formatWith(observationReferenceStrings))
}


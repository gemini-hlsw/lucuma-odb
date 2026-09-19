// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data
package arb

import lucuma.core.enums.AltairMode
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.CassRotator
import lucuma.core.enums.FieldLens
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.odb.data.AltairConfiguration
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

trait ArbAltairConfiguration:

  given Arbitrary[AltairConfiguration] =
    Arbitrary:
      for
        mode        <- arbitrary[AltairMode]
        fieldLens   <- arbitrary[Option[FieldLens]]
        cassRotator <- arbitrary[CassRotator]
        ndFilter    <- arbitrary[AltairNdFilter]
      yield AltairConfiguration(mode, fieldLens, cassRotator, ndFilter)

object ArbAltairConfiguration extends ArbAltairConfiguration

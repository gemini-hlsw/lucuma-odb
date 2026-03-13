// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input
package arb

import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset.given
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.arb.ArbExposureTimeMode.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.odb.data.Nullable
import lucuma.odb.data.arb.ArbNullable.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbIgrins2LongSlitInput:

  given Arbitrary[Igrins2LongSlitInput.Create] =
    Arbitrary:
      for
        e  <- arbitrary[Option[ExposureTimeMode]]
        om <- arbitrary[Option[Igrins2OffsetMode]]
        sv <- arbitrary[Option[Boolean]]
        so <- arbitrary[Option[List[Offset]]]
      yield Igrins2LongSlitInput.Create(e, om, sv, so)

  given Arbitrary[Igrins2LongSlitInput.Edit] =
    Arbitrary:
      for
        e  <- arbitrary[Option[ExposureTimeMode]]
        om <- arbitrary[Nullable[Igrins2OffsetMode]]
        sv <- arbitrary[Nullable[Boolean]]
        so <- arbitrary[Nullable[List[Offset]]]
      yield Igrins2LongSlitInput.Edit(e, om, sv, so)

object ArbIgrins2LongSlitInput extends ArbIgrins2LongSlitInput

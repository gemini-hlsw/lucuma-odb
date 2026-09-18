// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.syntax.option.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.CassRotator
import lucuma.core.enums.FieldLens
import lucuma.odb.data.AltairConfiguration
import lucuma.odb.sequence.data.arb.ArbGeneratorParams.given
import lucuma.odb.sequence.util.HashBytes
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class GeneratorParamsSuite extends ScalaCheckSuite:

  private val ngs: AltairConfiguration =
    AltairConfiguration(AltairMode.Ngs, none, CassRotator.Following, AltairNdFilter.Out)

  private def hash(params: GeneratorParams): List[Byte] =
    HashBytes[GeneratorParams].md5(params).toList

  property("every part of the Altair configuration changes the hash"):
    forAll { (params: GeneratorParams) =>
      // Altair fixes the guide probe and feeds the ITC, so a stored obscalc result computed for
      // one configuration must never be served for another.
      val withNgs = hash(params.copy(altair = ngs.some))

      assertNotEquals(withNgs, hash(params.copy(altair = none)))
      assertNotEquals(withNgs, hash(params.copy(altair = ngs.copy(mode = AltairMode.Lgs).some)))
      assertNotEquals(withNgs, hash(params.copy(altair = ngs.copy(explicitFieldLens = FieldLens.Out.some).some)))
      assertNotEquals(withNgs, hash(params.copy(altair = ngs.copy(cassRotator = CassRotator.Fixed).some)))
      assertNotEquals(withNgs, hash(params.copy(altair = ngs.copy(ndFilter = AltairNdFilter.In).some)))
    }

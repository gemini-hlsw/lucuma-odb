// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import lucuma.odb.data.OdbError

class ImagingFilterCheckSuite extends munit.FunSuite:

  private def detail(e: OdbError): String =
    e match
      case OdbError.InvalidArgument(d) => d.getOrElse("")
      case _                           => ""

  test("instrumentName is resolved (not null) in the error message"):
    assert(detail(GmosImagingInput.atLeastOne).contains("GMOS imaging"), detail(GmosImagingInput.atLeastOne))
    assert(detail(Flamingos2ImagingInput.atLeastOne).contains("Flamingos2 imaging"), detail(Flamingos2ImagingInput.atLeastOne))

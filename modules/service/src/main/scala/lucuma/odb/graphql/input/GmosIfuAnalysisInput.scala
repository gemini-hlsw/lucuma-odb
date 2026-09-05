// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.math.Angle
import lucuma.core.model.GmosIfuAnalysis
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*

/**
 * How the ITC samples a GMOS IFU field: exactly one of a summation radius or a single element
 * offset (`@oneOf`).  The number of fibres on sky is not settable, since it follows from the
 * focal plane unit.
 */
object GmosIfuAnalysisInput:

  val Binding: Matcher[GmosIfuAnalysis] =
    ObjectFieldsBinding.rmap:
      case List(
        AngleInput.Binding.Option("sumRadius", rSumRadius),
        AngleInput.Binding.Option("singleOffset", rSingleOffset)
      ) =>
        (rSumRadius, rSingleOffset).parTupled.flatMap: (sumRadius, singleOffset) =>
          oneOrFail(
            sumRadius.map(GmosIfuAnalysis.Sum(_))       -> "sumRadius",
            singleOffset.map(GmosIfuAnalysis.Single(_)) -> "singleOffset"
          ).flatMap:
            // At or below zero the legacy ITC recipe builds no apertures at all and then indexes
            // into them, failing with a bare IndexOutOfBounds.  Compare the signed value: `Angle`
            // is modular, so a negative arrives as a near-360-degree radius that would silently
            // sum the whole field.
            case GmosIfuAnalysis.Sum(radius) if arcsec(radius) <= 0 =>
              OdbError
                .InvalidArgument:
                  s"The IFU summation radius must be greater than zero, got ${arcsec(radius)} arcsec.".some
                .asFailure

            case analysis                                           =>
              Result(analysis)

  private def arcsec(a: Angle): BigDecimal =
    Angle.signedDecimalArcseconds.get(a)

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.math.Angle
import lucuma.core.model.GmosIfuAnalysis
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

// How to sample a GMOS IFU field: exactly one of a summation radius or a single element
// offset (`@oneOf`).
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
            // At or below zero the legacy recipe builds no apertures at all and then indexes
            // into them, so it fails with a bare IndexOutOfBounds rather than a useful message.
            // Compare the signed value: `Angle` is modular, so a negative input arrives as a
            // near-360-degree radius that would silently sum the whole field.
            case GmosIfuAnalysis.Sum(radius) if arcsec(radius) <= 0 =>
              Result.failure:
                s"IFU summation radius must be greater than zero, got ${arcsec(radius)} arcsec."
            case analysis                                           =>
              Result.success(analysis)

  private def arcsec(a: Angle): BigDecimal =
    Angle.signedDecimalArcseconds.get(a)

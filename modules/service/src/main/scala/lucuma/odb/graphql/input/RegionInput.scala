// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.math.Arc
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.odb.graphql.binding.*

/**
 * The region a Target of Opportunity is approved for.
 *
 * An unspecified arc means unconstrained rather than missing, so on create it defaults to the
 * full circle: a target that names no region at all is approved for the whole sky, and one that
 * names an arc in only one axis is unconstrained in the other. That makes the whole region
 * omissible without a separate "no region" state to carry around, and a full-sky region is
 * trivially satisfied, so nothing downstream has to special-case its absence.
 *
 * Neither arc may be null. An arc that is present says what is approved; there is no third thing
 * for null to mean that omission does not already say better.
 */
object RegionInput:

  case class Create(raArc: Arc[RightAscension], decArc: Arc[Declination])
  case class Edit(raArc: Option[Arc[RightAscension]], decArc: Option[Arc[Declination]])

  /** The whole sky: what an opportunity target is approved for when it says nothing. */
  val Default: Create = Create(Arc.Full(), Arc.Full())

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap:
      case List(
        RightAscensionArcInput.Binding.NonNullable("rightAscensionArc", rRA),
        DeclinationArcInput.Binding.NonNullable("declinationArc", rDec)
      ) =>
        (rRA, rDec).mapN: (ra, dec) =>
          Create(ra.getOrElse(Default.raArc), dec.getOrElse(Default.decArc))

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        RightAscensionArcInput.Binding.NonNullable("rightAscensionArc", rRA),
        DeclinationArcInput.Binding.NonNullable("declinationArc", rDec)
      ) => (rRA, rDec).mapN(Edit.apply)

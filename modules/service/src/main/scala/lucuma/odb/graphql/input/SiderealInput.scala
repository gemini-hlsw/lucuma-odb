// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object SiderealInput {

  final case class Create(
    ra: RightAscension,
    dec: Declination,
    epoch: Epoch,
    properMotion: Option[ProperMotion],
    radialVelocity: Option[RadialVelocity],
    parallax: Option[Parallax],
    catalogInfo: Option[CatalogInfoInput]
  )

  final case class Edit(
    ra: Nullable[RightAscension],
    dec: Nullable[Declination],
    epoch: Nullable[Epoch],
    properMotion: Nullable[ProperMotion],
    radialVelocity: Nullable[RadialVelocity],
    parallax: Nullable[Parallax],
    catalogInfo: Nullable[CatalogInfoInput]
  )

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap {
      case List(
        RightAscensionInput.Binding.Nullable("ra", rRa),
        DeclinationInput.Binding.Nullable("dec", rDec),
        EpochBinding.Nullable("epoch", rEpoch),
        ProperMotionInput.Binding.Nullable("properMotion", rProperMotion),
        RadialVelocityInput.Binding.Nullable("radialVelocity", rRadialVelocity),
        ParallaxModelInput.Binding.Nullable("parallax", rParallax),
        CatalogInfoInput.Binding.Nullable("catalogInfo", rCatalogInfo)
      ) =>
        (rRa, rDec, rEpoch, rProperMotion, rRadialVelocity, rParallax, rCatalogInfo).parMapN {
          Edit(_, _, _, _, _, _, _)
        }
    }
    
  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap {
      case List(
        RightAscensionInput.Binding.Option("ra", rRa),
        DeclinationInput.Binding.Option("dec", rDec),
        EpochBinding.Option("epoch", rEpoch),
        ProperMotionInput.Binding.Option("properMotion", rProperMotion),
        RadialVelocityInput.Binding.Option("radialVelocity", rRadialVelocity),
        ParallaxModelInput.Binding.Option("parallax", rParallax),
        CatalogInfoInput.Binding.Option("catalogInfo", rCatalogInfo)
      ) =>
        (rRa, rDec, rEpoch, rProperMotion, rRadialVelocity, rParallax, rCatalogInfo).parTupled.flatMap {
          (ra, dec, epoch, pm, rv, px, ci) =>
            (ra, dec, epoch) match {
              case (Some(r), Some(d), Some(e)) => Result(Create(r, d, e, pm, rv, px, ci))
              case _ => Matcher.validationFailure("RA, Dec, and Epoch must all be specified on target creation.")            
            }          
        }
    }
}



// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.math.Region
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

case class OpportunityInput(region: Region)

object OpportunityInput:

  case class Create(region: RegionInput.Create, resolution: Option[TargetResolutionInput.Create])

  /**
   * The two fields differ in exactly the way their meanings differ.
   *
   * The resolution is `Nullable`: unresolved is a real, reachable state, so assigning null
   * un-resolves the target and returns it to waiting, while omitting it leaves the resolution
   * alone.
   *
   * The region is optional but *not* nullable. Every opportunity target has one -- omitting it on
   * create approves the whole sky rather than leaving it unset -- so there is no state for null to
   * denote; ceasing to be a Target of Opportunity is a subtype change, made through the top-level
   * sidereal / nonsidereal fields. Omitting it here leaves the approved region untouched, which is
   * what makes resolving a target safe: a client that had to restate the region in order to
   * resolve could silently redraw it.
   *
   * GraphQL cannot express "optional but not nullable" in the type -- an input field is either
   * required or nullable -- so it is enforced here, by `NonNullable`.
   */
  case class Edit(region: Option[RegionInput.Edit], resolution: Nullable[TargetResolutionInput.Edit])

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap:
      case List(
        RegionInput.CreateBinding.NonNullable("region", rRegion),
        TargetResolutionInput.CreateBinding.Option("resolution", rResolution)
      ) =>
        (rRegion, rResolution).parMapN: (region, resolution) =>
          Create(region.getOrElse(RegionInput.Default), resolution)

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        RegionInput.EditBinding.NonNullable("region", rRegion),
        TargetResolutionInput.EditBinding.Nullable("resolution", rResolution)
      ) =>
        (rRegion, rResolution).parMapN(Edit.apply)

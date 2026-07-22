// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.TypeRef

import table.GoaDuplicationView
import table.GoaMatchView
import table.ObservationView

trait GoaDuplicationMapping[F[_]]
  extends GoaDuplicationView[F]
     with GoaMatchView[F]
     with ObservationView[F]:

  lazy val GoaDuplicationMapping: ObjectMapping =
    ObjectMapping(GoaDuplicationType)(
      SqlField("id", GoaDuplicationView.ObservationId, key = true, hidden = true),
      SqlField("state", GoaDuplicationView.State),
      SqlField("matchCount", GoaDuplicationView.MatchCount),
      SqlField("saturated", GoaDuplicationView.Saturated),
      SqlField("lastCheckedAt", GoaDuplicationView.LastCheckedAt),
      SqlField("error", GoaDuplicationView.Error),
      SqlObject("searchCoordinates"),
      SqlField("searchTargetName", GoaDuplicationView.SearchTargetName),
      SqlObject("searchRadius"),
      SqlObject("matches", Join(GoaDuplicationView.ObservationId, GoaMatchView.ObservationId))
    )

  lazy val GoaMatchMapping: ObjectMapping =
    ObjectMapping(GoaMatchType)(
      SqlField("id", GoaMatchView.Id, key = true, hidden = true),
      SqlField("name", GoaMatchView.Name),
      SqlField("dataLabel", GoaMatchView.DataLabel),
      SqlObject("coordinates"),
      SqlField("instrument", GoaMatchView.Instrument),
      SqlField("observationType", GoaMatchView.ObservationType),
      SqlField("observationClass", GoaMatchView.ObservationClass),
      SqlField("qaState", GoaMatchView.QaState),
      SqlField("utDateTime", GoaMatchView.UtDateTime),
      SqlField("releaseDate", GoaMatchView.ReleaseDate),
      SqlField("programId", GoaMatchView.ProgramId),
      SqlField("observationId", GoaMatchView.GoaObservationId),
      SqlField("objectName", GoaMatchView.ObjectName),
      SqlObject("exposure"),
      SqlField("disperser", GoaMatchView.Disperser),
      SqlField("filter", GoaMatchView.Filter),
      SqlObject("wavelength"),
      SqlField("airmass", GoaMatchView.Airmass),
      SqlObject("azimuth"),
      SqlObject("elevation"),
      SqlObject("distance")
    )

  lazy val GoaDuplicationElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GoaDuplicationType, "matches", Nil) =>
      Elab.transformChild: child =>
        OrderBy(OrderSelections(List(OrderSelection[String](GoaMatchType / "name"))), child)

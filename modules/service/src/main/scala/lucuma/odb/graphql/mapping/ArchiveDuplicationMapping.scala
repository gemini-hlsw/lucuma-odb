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

import table.ArchiveDuplicationView
import table.ArchiveMatchView
import table.ObservationView

trait ArchiveDuplicationMapping[F[_]]
  extends ArchiveDuplicationView[F]
     with ArchiveMatchView[F]
     with ObservationView[F]:

  lazy val ArchiveDuplicationMapping: ObjectMapping =
    ObjectMapping(ArchiveDuplicationType)(
      SqlField("id", ArchiveDuplicationView.ObservationId, key = true, hidden = true),
      SqlField("state", ArchiveDuplicationView.State),
      SqlField("matchCount", ArchiveDuplicationView.MatchCount),
      SqlField("saturated", ArchiveDuplicationView.Saturated),
      SqlField("lastCheckedAt", ArchiveDuplicationView.LastCheckedAt),
      SqlField("error", ArchiveDuplicationView.Error),
      SqlObject("searchCoordinates"),
      SqlField("searchTargetName", ArchiveDuplicationView.SearchTargetName),
      SqlObject("searchRadius"),
      SqlObject("matches", Join(ArchiveDuplicationView.ObservationId, ArchiveMatchView.ObservationId))
    )

  lazy val ArchiveMatchMapping: ObjectMapping =
    ObjectMapping(ArchiveMatchType)(
      SqlField("id", ArchiveMatchView.Id, key = true, hidden = true),
      SqlField("name", ArchiveMatchView.Name),
      SqlField("dataLabel", ArchiveMatchView.DataLabel),
      SqlObject("coordinates"),
      SqlField("instrument", ArchiveMatchView.Instrument),
      SqlField("observationType", ArchiveMatchView.ObservationType),
      SqlField("observationClass", ArchiveMatchView.ObservationClass),
      SqlField("qaState", ArchiveMatchView.QaState),
      SqlField("utDateTime", ArchiveMatchView.UtDateTime),
      SqlField("releaseDate", ArchiveMatchView.ReleaseDate),
      SqlField("programId", ArchiveMatchView.ProgramId),
      SqlField("observationId", ArchiveMatchView.GoaObservationId),
      SqlField("objectName", ArchiveMatchView.ObjectName),
      SqlObject("exposure"),
      SqlField("disperser", ArchiveMatchView.Disperser),
      SqlField("filter", ArchiveMatchView.Filter),
      SqlObject("wavelength"),
      SqlField("airmass", ArchiveMatchView.Airmass),
      SqlObject("azimuth"),
      SqlObject("elevation"),
      SqlObject("distance")
    )

  lazy val ArchiveDuplicationElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (ArchiveDuplicationType, "matches", Nil) =>
      Elab.transformChild: child =>
        OrderBy(OrderSelections(List(OrderSelection[String](ArchiveMatchType / "name"))), child)

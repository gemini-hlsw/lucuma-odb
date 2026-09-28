// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.all.*
import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import io.circe.Json
import io.circe.syntax.*
import lucuma.catalog.goa.GoaEndpoint
import lucuma.catalog.goa.GoaParams
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.syntax.string.*
import lucuma.odb.goa
import org.http4s.Uri

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
      SqlField("stale", ArchiveDuplicationView.Stale),
      SqlField("matchCount", ArchiveDuplicationView.MatchCount),
      SqlField("saturated", ArchiveDuplicationView.Saturated),
      SqlField("lastCheckedAt", ArchiveDuplicationView.LastCheckedAt),
      SqlField("attemptedAt", ArchiveDuplicationView.LastAttemptedAt),
      SqlField("error", ArchiveDuplicationView.Error),
      SqlObject("searchCoordinates"),
      SqlField("searchTargetName", ArchiveDuplicationView.SearchTargetName),
      SqlObject("searchRadius"),
      SqlField("queryUrls", ArchiveDuplicationView.QueryUrls),
      CursorFieldJson(
        "queries",
        _.fieldAs[List[String]]("queryUrls").map(_.mapFilter(archiveQuery).asJson),
        List("queryUrls")
      ),
      SqlObject("matches", Join(ArchiveDuplicationView.ObservationId, ArchiveMatchView.ObservationId))
    )

  /** Every stored query URL was built by `GoaParams.toUri`, so both parts are recoverable. */
  private def archiveQuery(queryUrl: String): Option[Json] =
    Uri.fromString(queryUrl).toOption.flatMap: uri =>
      GoaParams.instrumentOf(uri).map: instrument =>
        val searchUrl = GoaEndpoint.fromUri.replace(GoaEndpoint.SearchForm)(uri)
        Json.obj(
          "instrument" -> instrument.tag.toScreamingSnakeCase.asJson,
          "searchUrl"  -> searchUrl.renderString.asJson
        )

  lazy val ArchiveMatchMapping: ObjectMapping =
    ObjectMapping(ArchiveMatchType)(
      SqlField("id", ArchiveMatchView.Id, key = true, hidden = true),
      SqlField("name", ArchiveMatchView.Name),
      SqlField("dataLabel", ArchiveMatchView.DataLabel),
      SqlObject("coordinates"),
      SqlField("instrumentString", ArchiveMatchView.Instrument),
      CursorField[Option[Instrument]](
        "instrument",
        _.fieldAs[String]("instrumentString").map(goa.instrument),
        List("instrumentString")
      ),
      SqlField("observeClassString", ArchiveMatchView.ObservationClass),
      CursorField[Option[ObserveClass]](
        "observeClass",
        _.fieldAs[Option[String]]("observeClassString").map(_.flatMap(goa.observeClass)),
        List("observeClassString")
      ),
      SqlField("qaStateString", ArchiveMatchView.QaState),
      CursorField[Option[DatasetQaState]](
        "qaState",
        _.fieldAs[Option[String]]("qaStateString").map(_.flatMap(goa.qaState)),
        List("qaStateString")
      ),
      SqlField("utDateTime", ArchiveMatchView.UtDateTime),
      SqlField("releaseDate", ArchiveMatchView.ReleaseDate),
      SqlField("programReference", ArchiveMatchView.ProgramId),
      SqlField("observationReference", ArchiveMatchView.GoaObservationId),
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

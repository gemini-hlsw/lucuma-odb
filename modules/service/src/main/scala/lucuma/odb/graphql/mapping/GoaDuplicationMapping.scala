// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.all.*
import grackle.Cursor
import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension

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
      SqlObject("ra"),
      SqlObject("dec"),
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
      SqlField("distanceRa", GoaMatchView.Distance.Ra, hidden = true),
      SqlField("distanceDec", GoaMatchView.Distance.Dec, hidden = true),
      SqlField("searchRa", GoaMatchView.Distance.SearchRa, hidden = true),
      SqlField("searchDec", GoaMatchView.Distance.SearchDec, hidden = true),
      CursorField[Option[BigDecimal]](
        "distanceArcsec",
        distanceArcsec,
        List("distanceRa", "distanceDec", "searchRa", "searchDec")
      )
    )

  /**
   * Angular separation between a match and the center it was found around.
   * Absent when the search ran by target name, so there is no center to measure
   * from, or when the archived file records no pointing.
   */
  private def distanceArcsec(c: Cursor): Result[Option[BigDecimal]] =
    for
      ra        <- c.fieldAs[Option[RightAscension]]("distanceRa")
      dec       <- c.fieldAs[Option[Declination]]("distanceDec")
      searchRa  <- c.fieldAs[Option[RightAscension]]("searchRa")
      searchDec <- c.fieldAs[Option[Declination]]("searchDec")
    yield (ra, dec, searchRa, searchDec).mapN: (r, d, sr, sd) =>
      val separation = Coordinates(sr, sd).angularDistance(Coordinates(r, d))
      BigDecimal(separation.toMicroarcseconds) / 1_000_000

  lazy val GoaDuplicationElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GoaDuplicationType, "matches", Nil) =>
      Elab.transformChild: child =>
        OrderBy(OrderSelections(List(OrderSelection[String](GoaMatchType / "name"))), child)

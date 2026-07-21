// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.all.*
import grackle.Result
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.json.offset.query.given

/**
 * Shared encoding of the GraphQL `SlitTelescopeConfigs` output type from the two DB columns
 * used to persist it:
 * - a discriminant tag (`c_slit_offset_mode`) 
 * - and a JSON blob (`c_telescope_configs`). 
 *
 * Mixed into every long-slit mapping that exposes telescope configs (GNIRS, Flamingos2,
 * IGRINS-2) so the JSON shaping and cursor-field wiring live in one place. GMOS is pending.
 */
trait SlitTelescopeConfigsMapping[F[_]] extends BaseMapping[F]:

  private def telescopeConfigJson(offset: Offset, guiding: StepGuideState): Json =
    Json.obj(
      "offset"  -> Json.obj("p" -> offset.p.asJson, "q" -> offset.q.asJson),
      "guiding" -> guiding.asJson
    )

  private def telescopeConfigAlongSlitJson(q: Offset.Q, guiding: StepGuideState): Json =
    Json.obj(
      "q"       -> q.asJson,
      "guiding" -> guiding.asJson
    )

  /**
   * Encodes a `SlitTelescopeConfigs` as the GraphQL `SlitTelescopeConfigs` type:
   * `{ offsetMode, alongSlit, toSky }` with exactly one of `alongSlit` / `toSky` non-null,
   * depending on the discriminant. 
   *
   * Fails (rather than silently emitting `null`) when the persisted blob does not parse.
   */
  protected def slitTelescopeConfigsJson(mode: SlitOffsetMode, json: String): Result[Json] =
    SlitTelescopeConfigsFormat.getOption((mode, json)) match
      case Some(SlitTelescopeConfigs.AlongSlit(nel)) =>
        Result(Json.obj(
          "offsetMode" -> mode.asJson,
          "alongSlit"  -> nel.toList.map(c => telescopeConfigAlongSlitJson(c.offset, c.guiding)).asJson,
          "toSky"      -> Json.Null
        ))
      case Some(SlitTelescopeConfigs.ToSky(nel)) =>
        Result(Json.obj(
          "offsetMode" -> mode.asJson,
          "alongSlit"  -> Json.Null,
          "toSky"      -> nel.toList.map(tc => telescopeConfigJson(tc.offset, tc.guiding)).asJson
        ))
      case None =>
        Result.failure(s"Could not parse persisted telescope configs '$json' (mode ${mode.tag}).")

  /**
   * Encodes a plain `[TelescopeConfig]` blob (no slit offset mode) — used by GNIRS IFU, whose
   * configs are stored as a single value with no default / explicit split. Fails on a blob that
   * does not parse.
   */
  protected def ifuTelescopeConfigsJson(json: String): Result[Json] =
    ToSkyFormat.getOption(json) match
      case Some(nel) => Result(nel.toList.map(tc => telescopeConfigJson(tc.offset, tc.guiding)).asJson)
      case None      => Result.failure(s"Could not parse persisted telescope configs '$json'.")

  /**
   * Cursor field for an effective/default `SlitTelescopeConfigs!`. The mode column may be null
   * (e.g. a shared view row that is not a long slit), in which case the field resolves to `null`;
   * otherwise the blob is decoded and re-encoded, failing on a blob that does not parse.
   */
  protected def slitTelescopeConfigsField(name: String, modeCol: String, tcCol: String): CursorFieldJson =
    CursorFieldJson(
      name,
      cursor =>
        for
          mode <- cursor.field(modeCol, None).flatMap(_.as[Option[SlitOffsetMode]])
          tc   <- cursor.field(tcCol, None).flatMap(_.as[String])
          json <- mode.fold(Result(Json.Null))(slitTelescopeConfigsJson(_, tc))
        yield json,
      List(modeCol, tcCol)
    )

  /**
   * Cursor field for a nullable explicit `SlitTelescopeConfigs`. Present only when an explicit
   * override is set (both columns non-null, enforced by a DB both-or-neither CHECK); resolves to
   * `null` otherwise.
   */
  protected def explicitSlitTelescopeConfigsField(name: String, modeCol: String, tcCol: String): CursorFieldJson =
    CursorFieldJson(
      name,
      cursor =>
        for
          modeOpt <- cursor.field(modeCol, None).flatMap(_.as[Option[SlitOffsetMode]])
          jsonOpt <- cursor.field(tcCol, None).flatMap(_.as[Option[String]])
          json    <- (modeOpt, jsonOpt).mapN(slitTelescopeConfigsJson).fold(Result(Json.Null))(identity)
        yield json,
      List(modeCol, tcCol)
    )

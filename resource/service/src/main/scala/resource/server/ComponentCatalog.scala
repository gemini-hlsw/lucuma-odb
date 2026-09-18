// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server

import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.odb.data.Existence
import resource.model.InstrumentComponentType
import resource.model.ResourceInstrument
import resource.server.Codecs.*
import skunk.codec.text.varchar

/**
 * One instrument component, as the night projection reads and encodes it.
 *
 * The `components` query does not come through here: it is served from
 * `v_instrument_component_at_site` by the SQL mapping. This row exists because the night projection
 * joins the catalog itself, which Grackle cannot express.
 */
object ComponentCatalog:

  final case class ComponentRow(
    id:            String,
    instrument:    ResourceInstrument,
    componentType: InstrumentComponentType,
    code:          NonEmptyString,
    name:          NonEmptyString,
    barcode:       Option[NonEmptyString],
    aliases:       List[NonEmptyString],
    existence:     Existence
  )

  val componentRowCodec: skunk.Codec[ComponentRow] =
    (varchar *: resource_instrument *: instrument_component_type *: text_nonempty *: text_nonempty *: text_nonempty.opt *: nonempty_text_array *: existence)
      .to[ComponentRow]

  /** The InstrumentComponent shape, which must match what the SQL mapping produces. */
  def componentJson(r: ComponentRow): Json =
    Json.obj(
      "id"            -> Json.fromString(r.id),
      "instrument"    -> r.instrument.asJson,
      "componentType" -> r.componentType.asJson,
      "code"          -> r.code.asJson,
      "name"          -> r.name.asJson,
      "barcode"       -> r.barcode.asJson,
      "aliases"       -> r.aliases.asJson,
      "existence"     -> r.existence.asJson
    )

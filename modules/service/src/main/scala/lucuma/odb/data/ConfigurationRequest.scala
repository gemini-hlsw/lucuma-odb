// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.util.Enumerated
import lucuma.core.util.WithGid
import lucuma.refined.*

case class ConfigurationRequest(
  id: ConfigurationRequest.Id,
  status: ConfigurationRequest.Status,
  configuration: Configuration
)
object ConfigurationRequest extends WithGid('x'.refined) {

  enum Status derives Enumerated:
    case Requested, Approved, Denied
    def tag = toString.toLowerCase // ???

  given Decoder[ConfigurationRequest] = hc =>
    for
      id <- hc.downField("id").as[Id]
      st <- hc.downField("status").as[Status]
      cf <- hc.downField("configuration").as[Configuration]
    yield apply(id, st, cf)

  given Encoder[ConfigurationRequest] = cr =>
    Json.obj(
      "id" -> cr.id.asJson,
      "status" -> cr.status.asJson,
      "configuration" -> cr.configuration.asJson
    )

}


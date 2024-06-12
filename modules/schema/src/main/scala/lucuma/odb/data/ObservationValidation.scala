// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.data.NonEmptyChain
import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.util.Enumerated

enum ObservationValidationCode(
  val tag: String,
  val name: String,
  val description: String
) derives Enumerated:
  case ConfigurationError extends ObservationValidationCode("configuration_error", "Configuration Error", "The observation is not configured correctly and cannot be executed")
  case CallForProposalsError extends ObservationValidationCode("cfp_error", "Call for Proposals Error", "Not valid for the selected Call for Proposals")

case class ObservationValidation(
  code: ObservationValidationCode,
  messages: NonEmptyChain[String]
) derives Codec

object ObservationValidation:
  def fromMsgs(code: ObservationValidationCode, msg: String, moreMsgs: String*): ObservationValidation =
    ObservationValidation(code, NonEmptyChain.of(msg, moreMsgs*))
  def configuration(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ObservationValidationCode.ConfigurationError, msg, moreMsgs*)
  def callForProposals(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ObservationValidationCode.CallForProposalsError, msg, moreMsgs*)

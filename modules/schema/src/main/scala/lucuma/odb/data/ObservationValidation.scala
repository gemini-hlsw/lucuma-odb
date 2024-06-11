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
  case MissingData extends ObservationValidationCode("missing_data", "Missing Data", "Some required data is missing")
  case ConflictingData extends ObservationValidationCode("conflicting_data", "Conflicting Data", "Conflicting data")
  case CallForProposalsError extends ObservationValidationCode("cfp_error", "CfP Error", "Not valid for the selected Call for Proposals")
  // we should never get this one, unless we change how the validation method is called.
  case MissingObservation extends ObservationValidationCode("missing_observation", "Missing Observation", "The observation is missing or not accessible")

case class ObservationValidation(
  code: ObservationValidationCode,
  messages: NonEmptyChain[String]
) derives Codec

object ObservationValidation:
  def fromMsgs(code: ObservationValidationCode, msg: String, moreMsgs: String*): ObservationValidation =
    ObservationValidation(code, NonEmptyChain.of(msg, moreMsgs*))
  def missingData(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ObservationValidationCode.MissingData, msg, moreMsgs*)
  def conflictingData(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ObservationValidationCode.ConflictingData, msg, moreMsgs*)
  def callForProposals(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ObservationValidationCode.CallForProposalsError, msg, moreMsgs*)
  def missingObservation(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ObservationValidationCode.MissingObservation, msg, moreMsgs*)

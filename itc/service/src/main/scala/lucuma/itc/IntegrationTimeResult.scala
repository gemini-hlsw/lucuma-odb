// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import io.circe.Encoder

sealed trait IntegrationTimeError extends RuntimeException {
  def message: String

  override def getMessage(): String = message
}

object IntegrationTimeError {
  def unapply(e: IntegrationTimeError): Option[String] = Some(e.message)
}

case class SourceTooBright(wellHalfFilledSeconds: BigDecimal) extends IntegrationTimeError
    derives Encoder.AsObject {
  val message: String = f"Source too bright, well half filled in $wellHalfFilledSeconds%.2f seconds"
}

/** Generic calculation error */
case class CalculationError(msg: List[String]) extends IntegrationTimeError
    derives Encoder.AsObject {
  val message: String = msg.mkString("\n")
}

object CalculationError {
  def apply(msg: String): CalculationError = CalculationError(List(msg))
}

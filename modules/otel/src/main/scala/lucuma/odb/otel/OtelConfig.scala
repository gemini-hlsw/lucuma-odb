// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.otel

case class OtelConfig(
  endpoint:    String,
  instanceId:  String,
  apiKey:      String,
  environment: String
)

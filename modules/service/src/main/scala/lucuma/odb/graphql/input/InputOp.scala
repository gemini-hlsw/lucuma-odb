// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

sealed trait InputOp
object InputOp {
  object Create extends InputOp
  object Edit extends InputOp
}
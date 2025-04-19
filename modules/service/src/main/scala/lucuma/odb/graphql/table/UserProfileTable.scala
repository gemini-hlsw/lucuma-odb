// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

trait UserProfileTable[R]:
  def GivenName: R
  def FamilyName: R
  def CreditName: R
  def Email: R


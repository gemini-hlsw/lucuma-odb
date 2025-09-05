// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Target

class TargetPredicates(path: Path) {
  lazy val existence = ExistencePredicates(path / "existence")
  lazy val id = LeafPredicates[Target.Id](path / "id")
  lazy val program = ProgramPredicates(path / "program")

  def hasCalibrationRole(role: CalibrationRole): Predicate =
    Eql(path / "calibrationRole", Const[CalibrationRole](role))

  def hasCalibrationRole: Predicate =
    IsNull(path / "calibrationRole", false)

  def noCalibrationRole: Predicate =
    IsNull(path / "calibrationRole", true)

  lazy val targetDisposition = LeafPredicates[lucuma.core.enums.TargetDisposition](path / "targetDisposition")
}

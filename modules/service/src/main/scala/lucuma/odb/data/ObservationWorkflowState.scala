// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

sealed trait ObservationWorkflowState

sealed trait UserStatus extends ObservationWorkflowState
object UserStatus:
  case object Inactive extends UserStatus         // User can set status back to computed state, or to Ready if computed state is Defined.
  case object Ready extends UserStatus            // User can set status to Inactive or back to Defined.                 

sealed trait ExecutionStatus extends ObservationWorkflowState
object ExecutionStatus:
  case object Ongoing   extends ExecutionStatus  // User can set status to Inactive.                                                     
  case object Completed extends ExecutionStatus  // No transitions allowed.                                               

sealed trait ValidationStatus extends ObservationWorkflowState
object ValidationStatus:
  case object Undefined  extends ValidationStatus // User can set status to Inactive.                                                     
  case object Unapproved extends ValidationStatus // User can set status to Inactive.                                                     
  case object Defined    extends ValidationStatus // User can set status to Inactive, or Ready if proposal has been accepted.                                            



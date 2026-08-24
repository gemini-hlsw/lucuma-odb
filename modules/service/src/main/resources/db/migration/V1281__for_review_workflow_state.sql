-- ObservationWorkflowState.ForReview, reinstated in lucuma-core 0.236.1.
-- StartupDiagnostics compares each Scala Enumerated against its Postgres enum
-- and refuses to start on a missing tag, so the value has to exist even though
-- nothing yet puts an observation into this state.
--
-- Placed BEFORE 'ready' rather than appended: ForReview sits between Defined
-- and Ready in the Scala declaration, and e_workflow_state was created in that
-- same order, so this keeps the Postgres ordering -- which is what any
-- comparison or ORDER BY on the column sees -- identical to Scala's.

ALTER TYPE e_workflow_state ADD VALUE 'for_review' BEFORE 'ready';

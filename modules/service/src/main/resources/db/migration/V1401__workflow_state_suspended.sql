-- The Suspended workflow state: an observation that started executing but isn't
-- complete, and isn't to be considered for further execution unless and until
-- resumed.  See V1402.
--
-- On its own because a new enum value can't be used in the transaction that
-- adds it, and so that Flyway never sees ADD VALUE beside other statements.
ALTER TYPE e_workflow_state ADD VALUE 'suspended' AFTER 'ongoing';

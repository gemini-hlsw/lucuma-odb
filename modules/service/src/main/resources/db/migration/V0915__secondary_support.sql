
-- change support to support_primary
ALTER TYPE e_program_user_role RENAME VALUE 'support' TO 'support_primary';

-- add support_secondary
ALTER TYPE e_program_user_role ADD VALUE 'support_secondary' AFTER 'support_primary';


CREATE TYPE e_partner_association AS enum(
  'has_partner',
  'has_no_partner',
  'not_specified'
);

ALTER TABLE t_program_user
  ADD COLUMN c_association e_partner_association NOT NULL DEFAULT 'not_specified';

UPDATE TABLE t_program_user
SET c_association = 'has_partner'
WHERE c_partner IS NOT NULL;
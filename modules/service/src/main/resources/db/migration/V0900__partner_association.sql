CREATE TYPE e_partner_link AS ENUM (
  'has_partner',
  'has_no_partner',
  'unspecified_partner'
);

ALTER TABLE t_program_user
  ADD COLUMN c_partner_link e_partner_link;

UPDATE t_program_user
  SET c_partner_link =
    CASE
      WHEN c_partner IS NOT NULL THEN 'has_partner'::e_partner_link
      ELSE 'unspecified_partner'::e_partner_link
    END;

ALTER TABLE t_program_user
  ALTER COLUMN c_partner_link SET NOT NULL,
  ADD CONSTRAINT program_user_partner_check CHECK (
    (c_partner IS NOT NULL AND c_partner_link = 'has_partner') OR
    (c_partner IS NULL AND c_partner_link != 'has_partner')
  );

ALTER TABLE t_invitation
  ADD COLUMN c_partner_link e_partner_link;

UPDATE t_invitation
  SET c_partner_link =
    CASE
      WHEN c_partner IS NOT NULL THEN 'has_partner'::e_partner_link
      ELSE 'unspecified_partner'::e_partner_link
    END;

ALTER TABLE t_invitation
  ALTER COLUMN c_partner_link SET NOT NULL,
  ADD CONSTRAINT invitation_partner_check CHECK (
    (c_partner IS NOT NULL AND c_partner_link = 'has_partner') OR
    (c_partner IS NULL AND c_partner_link != 'has_partner')
  );
CREATE TYPE e_exchange_partner AS ENUM (
  'keck',
  'subaru'
);

CREATE TYPE e_keck_instrument AS ENUM (
  'hires',
  'other'
);

CREATE TYPE e_observatory AS ENUM (
  'gemini',
  'keck',
  'subaru'
);

CREATE TYPE e_subaru_instrument AS ENUM (
  'focas',
  'hds',
  'hsc',
  'ircs',
  'moircs',
  'pfs',
  'visitor'
);

CREATE TYPE e_subaru_proposal_type AS ENUM (
  'normal',
  'intensive'
);

-- A program user may be affiliated with an exchange partner (Keck/Subaru)
-- community rather than a Gemini partner.  Add the new partner link type.
ALTER TYPE e_partner_link ADD VALUE IF NOT EXISTS 'has_exchange_partner';

-- "has_partner" now specifically means a Gemini partner, so rename the value to
-- match.  This is an in-place relabel; existing rows retain their identity.
ALTER TYPE e_partner_link RENAME VALUE 'has_partner' TO 'has_gemini_partner';
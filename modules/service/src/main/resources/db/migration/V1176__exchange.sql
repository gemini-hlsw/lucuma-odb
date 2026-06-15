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

-- Exchange Call for Proposals types.  These must be added in a migration
-- separate from any that uses them (Postgres forbids using a newly-added enum
-- value in the same transaction that adds it), hence here in V1176 rather than
-- in V1177 where the t_cfp rework lives.
ALTER TYPE e_cfp_type ADD VALUE IF NOT EXISTS 'keck';
ALTER TYPE e_cfp_type ADD VALUE IF NOT EXISTS 'subaru';
ALTER TYPE e_cfp_type ADD VALUE IF NOT EXISTS 'subaru_intensive';

-- Corresponding science subtypes for exchange proposals.
ALTER TYPE e_science_subtype ADD VALUE IF NOT EXISTS 'keck';
ALTER TYPE e_science_subtype ADD VALUE IF NOT EXISTS 'subaru';
ALTER TYPE e_science_subtype ADD VALUE IF NOT EXISTS 'subaru_intensive';

-- Widen the science subtype abbreviation check to include the exchange letters.
ALTER TABLE t_science_subtype
  DROP CONSTRAINT t_science_subtype_c_abbr_check,
  ADD CONSTRAINT  t_science_subtype_c_abbr_check CHECK (c_abbr IN ('C', 'D', 'F', 'L', 'P', 'Q', 'S', 'V', 'K', 'U', 'I'));
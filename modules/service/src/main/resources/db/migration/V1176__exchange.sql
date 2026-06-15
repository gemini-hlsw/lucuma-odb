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
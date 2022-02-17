
create domain d_user_id as varchar; -- TODO format check

create type e_user_type as enum('guest', 'standard', 'service');

-- This is a write-through cache ... everything here is source from SSO
-- This table is for standard users.
create table t_user (

  c_user_id                d_user_id   primary key not null,
  c_user_type              e_user_type not null,
  c_service_name           VARCHAR, -- non-null iff c_user_type = 'service'
  c_orcid_id               VARCHAR UNIQUE, -- non-null iff c_user_type = 'standard'
  c_orcid_given_name       VARCHAR,
  c_orcid_credit_name      VARCHAR,
  c_orcid_family_name      VARCHAR,
  c_orcid_email            VARCHAR,

  -- service users must have a service name, others must not
  CHECK ((c_user_type =  'service' AND c_service_name IS NOT NULL)
      OR (c_user_type != 'service' AND c_service_name IS NULL)),

  -- guest users can't have an ORCID iD, but others must have one
  CHECK (((c_user_type =  'guest' OR  c_user_type = 'service') AND c_orcid_id IS NULL)
     OR   (c_user_type != 'guest' AND c_user_type != 'service' AND c_orcid_id IS NOT NULL)),

  -- required for composite fk elsewhere
  unique (c_user_id, c_user_type)

)


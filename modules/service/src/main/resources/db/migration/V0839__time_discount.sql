-- Discriminator for various time accounting charge discount types.
CREATE TYPE e_time_charge_discount_type AS ENUM (
  'daylight',
  'fault',
  'qa',
  'weather'
);

-- Time accounting charge discounts
CREATE TABLE t_time_charge_discount (

  c_id               bigserial   PRIMARY KEY,

  c_visit_id         d_visit_id  NOT NULL REFERENCES t_visit(c_visit_id) ON DELETE CASCADE,

  c_start            timestamp   NOT NULL,

  c_end              timestamp   NOT NULL,

  c_partner_discount interval    NOT NULL DEFAULT '0'::interval
    CHECK (c_partner_discount >= '0'::interval),

  c_program_discount interval    NOT NULL DEFAULT '0'::interval
    CHECK (c_program_discount >= '0'::interval),

  c_comment          text        NOT NULL,

  -- Discount type specific columns below
  c_type             e_time_charge_discount_type NOT NULL,

  -- Daylight discount
  c_site             e_site      NULL
    CHECK (c_type != 'daylight' OR c_site IS NOT NULL)

  -- TBD: fault report reference?
  -- TBD: weather issue?

);

CREATE INDEX time_charge_discount_visit_index ON t_time_charge_discount (c_visit_id);

CREATE TABLE t_time_charge_discount_dataset (

  c_discount_id int8         NOT NULL REFERENCES t_time_charge_discount(c_id) ON DELETE CASCADE,
  c_dataset_id  d_dataset_id NOT NULL REFERENCES t_dataset(c_dataset_id)      ON DELETE CASCADE,

  PRIMARY KEY (c_discount_id, c_dataset_id)

);
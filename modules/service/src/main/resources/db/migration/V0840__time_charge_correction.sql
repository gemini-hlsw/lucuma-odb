
-- Charge class categories
CREATE TYPE e_charge_class AS ENUM (
  'nonCharged',
  'partner',
  'program'
);

CREATE TYPE e_time_charge_correction_op AS ENUM (
  'add',
  'subtract'
);

-- Manual time accounting corrections
CREATE TABLE t_time_charge_correction (

  c_id           bigserial                   PRIMARY KEY,
  c_visit_id     d_visit_id                  NOT NULL REFERENCES t_visit(c_visit_id) ON DELETE CASCADE,
  c_created      timestamp                   NOT NULL DEFAULT now(),
  c_charge_class e_charge_class              NOT NULL,
  c_op           e_time_charge_correction_op NOT NULL,
  c_amount       interval                    NOT NULL CHECK (c_amount >= '0'::interval),
  c_user_id      d_user_id                   NOT NULL REFERENCES t_user(c_user_id),
  c_comment      text                        NULL CHECK (c_comment IS NULL OR length(c_comment) > 0)

);

CREATE INDEX time_charge_correction_visit_index ON t_time_charge_correction (c_visit_id);

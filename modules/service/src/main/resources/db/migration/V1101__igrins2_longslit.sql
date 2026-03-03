CREATE TABLE t_igrins_2_offset_mode (
  c_tag         d_tag NOT NULL PRIMARY KEY,
  c_description text  NOT NULL
);

INSERT INTO t_igrins_2_offset_mode VALUES
  ('nod_along_slit', 'Nod along slit'),
  ('nod_to_sky',     'Nod to sky');

--- IGRINS-2 Long Slit observing mode
CREATE OR REPLACE FUNCTION format_igrins_2_long_slit_mode_group(
  program_id              d_program_id,
  observing_mode_type     e_observing_mode_type,
  offset_mode             d_tag,
  offset_mode_default     d_tag,
  save_svc_images         boolean,
  save_svc_images_default boolean
) RETURNS text AS $$
DECLARE
BEGIN
  -- Concat all the fields together into a text value.  Here we use the default
  -- value when an explicit override is not present.
  RETURN concat_ws(
    ':',
    program_id::text,
    observing_mode_type::text,
    COALESCE(offset_mode, offset_mode_default)::text,
    COALESCE(save_svc_images, save_svc_images_default)::text
  );
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE TABLE t_igrins_2_long_slit (

  c_observation_id             d_observation_id      NOT NULL,
  c_program_id                 d_program_id          NOT NULL,
  c_instrument                 d_tag                 NOT NULL DEFAULT 'Igrins2'            REFERENCES t_instrument(c_tag) CHECK (c_instrument = 'Igrins2'),
  c_observing_mode_type        e_observing_mode_type NOT NULL DEFAULT 'igrins_2_long_slit'                                CHECK (c_observing_mode_type = 'igrins_2_long_slit'),

  c_offset_mode                d_tag                 NULL DEFAULT NULL                     REFERENCES t_igrins_2_offset_mode(c_tag),
  c_offset_mode_default        d_tag                 NOT NULL DEFAULT 'nod_along_slit'     REFERENCES t_igrins_2_offset_mode(c_tag),
  c_save_svc_images            boolean               NULL DEFAULT NULL,
  c_save_svc_images_default    boolean               NOT NULL DEFAULT false,

  c_mode_key                   text                  NOT NULL GENERATED ALWAYS AS (
    format_igrins_2_long_slit_mode_group(
      c_program_id,
      c_observing_mode_type,
      c_offset_mode,
      c_offset_mode_default,
      c_save_svc_images,
      c_save_svc_images_default
    )
  ) STORED,

  PRIMARY KEY (c_observation_id, c_instrument, c_observing_mode_type),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type) REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

-- View of igrins_2_long_slit observing modes
CREATE OR REPLACE VIEW v_igrins_2_long_slit AS
SELECT
  m.*
FROM
  t_igrins_2_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id;

-- Update v_observing_mode_group to include IGRINS-2
CREATE OR REPLACE VIEW v_observing_mode_group AS
-- GMOS-N LongSlit
  SELECT
    m.c_mode_key,
    o.c_program_id,
    max(m.c_observation_id) as c_observation_id
  FROM
    t_gmos_north_long_slit m
  LEFT JOIN t_observation o ON m.c_observation_id = o.c_observation_id
  GROUP BY
    m.c_mode_key,
    o.c_program_id
-- GMOS-S LongSlit
UNION ALL
  SELECT
    m.c_mode_key,
    o.c_program_id,
    max(m.c_observation_id) as c_observation_id
  FROM
    t_gmos_south_long_slit m
  LEFT JOIN t_observation o ON m.c_observation_id = o.c_observation_id
  GROUP BY
    m.c_mode_key,
    o.c_program_id
-- F2 LongSlit
UNION ALL
  SELECT
    m.c_mode_key,
    o.c_program_id,
    max(m.c_observation_id) as c_observation_id
  FROM
    t_flamingos_2_long_slit m
  LEFT JOIN t_observation o ON m.c_observation_id = o.c_observation_id
  GROUP BY
    m.c_mode_key,
    o.c_program_id
-- IGRINS-2 LongSlit
UNION ALL
  SELECT
    m.c_mode_key,
    o.c_program_id,
    max(m.c_observation_id) as c_observation_id
  FROM
    t_igrins_2_long_slit m
  LEFT JOIN t_observation o ON m.c_observation_id = o.c_observation_id
  GROUP BY
    m.c_mode_key,
    o.c_program_id;

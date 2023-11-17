-- GUIDE TARGET AVAILABILITY TABLES FOR CACHING

create table t_guide_availability ( 
  c_program_id              d_program_id      NOT NULL,
  c_observation_id          d_observation_id  NOT NULL,
  c_hash                    bytea             NOT NULL,

  FOREIGN KEY (c_program_id, c_observation_id)
    REFERENCES t_observation(c_program_id, c_observation_id)
    ON DELETE CASCADE,
  CONSTRAINT t_guide_availability_pkey PRIMARY KEY (c_program_id, c_observation_id)
 );

create table t_guide_availability_period ( 
  c_program_id              d_program_id      NOT NULL,
  c_observation_id          d_observation_id  NOT NULL,
  c_start                   timestamp         NOT NULL,
  c_end                     timestamp         NOT NULL,
  c_angles                  d_angle_µas[]     NOT NULL,

  FOREIGN KEY (c_program_id, c_observation_id)
    REFERENCES t_guide_availability(c_program_id, c_observation_id)
    ON DELETE CASCADE,
  CONSTRAINT t_guide_availability_period_pkey PRIMARY KEY (c_program_id, c_observation_id, c_start)
 );

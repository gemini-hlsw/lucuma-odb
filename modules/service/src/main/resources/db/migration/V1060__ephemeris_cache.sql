

CREATE DOMAIN d_ephemeris_key as varchar;

CREATE TABLE t_ephemeris (

  -- The key fields
  c_key_type    e_ephemeris_key_type  NOT null,
  c_des         varchar               NOT null,
  c_site        e_site                NOT NULL,
  c_when        timestamp             NOT NULL,
  unique (c_key_type, c_des, c_site, c_when),
  
  -- Position and velocity
  c_ra          d_angle_µas           NOT NULL,
  c_dec         d_angle_µas           NOT NULL,
  c_dra         d_angle_µas           NOT NULL,
  c_ddec        d_angle_µas           NOT NULL,

  -- Other information that we're not using yet, but let's hang onto it
  c_airmass     d_air_mass            NULL, 
  c_extinction  int2                  NULL, -- check?
  c_vmag        numeric               NULL, -- TODO: check constraint
  c_sb          numeric               NULL  -- TODO: check constraint
  
);

-- We search by time ranges so let's be sure we have an index.
CREATE INDEX ON t_ephemeris USING btree (c_when);





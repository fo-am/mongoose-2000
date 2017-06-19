-- Database generated with pgModeler (PostgreSQL Database Modeler).
-- pgModeler  version: 0.9.0-beta
-- PostgreSQL version: 9.6
-- Project Site: pgmodeler.com.br
-- Model Author: ---


-- Database creation must be done outside an multicommand file.
-- These commands were put in this file only for convenience.
-- -- object: "Foam_auto" | type: DATABASE --
-- -- DROP DATABASE IF EXISTS "Foam_auto";
-- CREATE DATABASE "Foam_auto"
-- ;
-- -- ddl-end --
-- 

-- object: mongoose | type: SCHEMA --
-- DROP SCHEMA IF EXISTS mongoose CASCADE;

-- Prepended SQL commands --
 CREATE EXTENSION Postgis;
-- ddl-end --

CREATE SCHEMA mongoose;
-- ddl-end --
ALTER SCHEMA mongoose OWNER TO postgres;
-- ddl-end --

SET search_path TO pg_catalog,public,mongoose;
-- ddl-end --

-- object: mongoose.pack | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.pack CASCADE;
CREATE TABLE mongoose.pack(
	pack_id serial NOT NULL,
	name text NOT NULL,
	pack_created_date timestamp,
	CONSTRAINT pack_pk PRIMARY KEY (pack_id),
	CONSTRAINT name_unique UNIQUE (name)

);
-- ddl-end --
ALTER TABLE mongoose.pack OWNER TO postgres;
-- ddl-end --

-- object: mongoose.litter | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.litter CASCADE;
CREATE TABLE mongoose.litter(
	litter_id serial NOT NULL,
	pack_id integer,
	name text NOT NULL,
	dateformed date,
	CONSTRAINT litter_pk PRIMARY KEY (litter_id),
	CONSTRAINT "UQ_Litter_name_unique" UNIQUE (name)

);
-- ddl-end --
ALTER TABLE mongoose.litter OWNER TO postgres;
-- ddl-end --

-- object: pack_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.litter DROP CONSTRAINT IF EXISTS pack_fk CASCADE;
ALTER TABLE mongoose.litter ADD CONSTRAINT pack_fk FOREIGN KEY (pack_id)
REFERENCES mongoose.pack (pack_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: mongoose.individual | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.individual CASCADE;
CREATE TABLE mongoose.individual(
	individual_id serial NOT NULL,
	litter_id integer,
	name text NOT NULL,
	sex text,
	CONSTRAINT individual_pk PRIMARY KEY (individual_id),
	CONSTRAINT unique_name UNIQUE (name)

);
-- ddl-end --
ALTER TABLE mongoose.individual OWNER TO postgres;
-- ddl-end --

-- object: litter_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.individual DROP CONSTRAINT IF EXISTS litter_fk CASCADE;
ALTER TABLE mongoose.individual ADD CONSTRAINT litter_fk FOREIGN KEY (litter_id)
REFERENCES mongoose.litter (litter_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: mongoose.pack_history | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.pack_history CASCADE;
CREATE TABLE mongoose.pack_history(
	pack_history_id serial NOT NULL,
	pack_id integer,
	individual_id integer,
	date_joined timestamp,
	CONSTRAINT packhistory_pk PRIMARY KEY (pack_history_id)

);
-- ddl-end --
ALTER TABLE mongoose.pack_history OWNER TO postgres;
-- ddl-end --

-- object: pack_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.pack_history DROP CONSTRAINT IF EXISTS pack_fk CASCADE;
ALTER TABLE mongoose.pack_history ADD CONSTRAINT pack_fk FOREIGN KEY (pack_id)
REFERENCES mongoose.pack (pack_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: individual_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.pack_history DROP CONSTRAINT IF EXISTS individual_fk CASCADE;
ALTER TABLE mongoose.pack_history ADD CONSTRAINT individual_fk FOREIGN KEY (individual_id)
REFERENCES mongoose.individual (individual_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: mongoose.interaction | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.interaction CASCADE;
CREATE TABLE mongoose.interaction(
	interaction_id serial NOT NULL,
	focalpack_id integer,
	secondpack_id integer,
	"time" timestamp NOT NULL,
	location geography,
	comment text,
	CONSTRAINT interaction_pk PRIMARY KEY (interaction_id)

);
-- ddl-end --
ALTER TABLE mongoose.interaction OWNER TO postgres;
-- ddl-end --

-- object: pack_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.interaction DROP CONSTRAINT IF EXISTS pack_fk CASCADE;
ALTER TABLE mongoose.interaction ADD CONSTRAINT pack_fk FOREIGN KEY (focalpack_id)
REFERENCES mongoose.pack (pack_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: mongoose.pack_event | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.pack_event CASCADE;
CREATE TABLE mongoose.pack_event(
	pack_event_id serial NOT NULL,
	pack_id integer,
	pack_event_code_id integer,
	date date NOT NULL,
	exact text,
	status text,
	location geography,
	comment text,
	CONSTRAINT event_pk PRIMARY KEY (pack_event_id)

);
-- ddl-end --
ALTER TABLE mongoose.pack_event OWNER TO postgres;
-- ddl-end --

-- object: pack_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.pack_event DROP CONSTRAINT IF EXISTS pack_fk CASCADE;
ALTER TABLE mongoose.pack_event ADD CONSTRAINT pack_fk FOREIGN KEY (pack_id)
REFERENCES mongoose.pack (pack_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: mongoose.pack_event_code | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.pack_event_code CASCADE;
CREATE TABLE mongoose.pack_event_code(
	pack_event_code_id serial NOT NULL,
	code text NOT NULL,
	detail text,
	CONSTRAINT packevnettypes_pk PRIMARY KEY (pack_event_code_id),
	CONSTRAINT uq_code_is_unique UNIQUE (code)

);
-- ddl-end --
ALTER TABLE mongoose.pack_event_code OWNER TO postgres;
-- ddl-end --

-- object: mongoose.individual_event | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.individual_event CASCADE;
CREATE TABLE mongoose.individual_event(
	individual_event_id serial NOT NULL,
	individual_event_code_id integer,
	individual_id integer,
	date date NOT NULL,
	exact text,
	status text,
	location geography,
	comment text,
	CONSTRAINT individualevent_pk PRIMARY KEY (individual_event_id)

);
-- ddl-end --
ALTER TABLE mongoose.individual_event OWNER TO postgres;
-- ddl-end --

-- object: individual_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.individual_event DROP CONSTRAINT IF EXISTS individual_fk CASCADE;
ALTER TABLE mongoose.individual_event ADD CONSTRAINT individual_fk FOREIGN KEY (individual_id)
REFERENCES mongoose.individual (individual_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: mongoose.individual_event_code | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.individual_event_code CASCADE;
CREATE TABLE mongoose.individual_event_code(
	individual_event_code_id serial NOT NULL,
	code text NOT NULL,
	CONSTRAINT individualeventcode_pk PRIMARY KEY (individual_event_code_id)

);
-- ddl-end --
ALTER TABLE mongoose.individual_event_code OWNER TO postgres;
-- ddl-end --

-- object: individual_event_code_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.individual_event DROP CONSTRAINT IF EXISTS individual_event_code_fk CASCADE;
ALTER TABLE mongoose.individual_event ADD CONSTRAINT individual_event_code_fk FOREIGN KEY (individual_event_code_id)
REFERENCES mongoose.individual_event_code (individual_event_code_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: mongoose.pregnancy | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.pregnancy CASCADE;
CREATE TABLE mongoose.pregnancy(
	pregnancy_id serial NOT NULL,
	individual_id integer,
	litter_id integer,
	CONSTRAINT pregnancy_pk PRIMARY KEY (pregnancy_id)

);
-- ddl-end --
ALTER TABLE mongoose.pregnancy OWNER TO postgres;
-- ddl-end --

-- object: individual_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.pregnancy DROP CONSTRAINT IF EXISTS individual_fk CASCADE;
ALTER TABLE mongoose.pregnancy ADD CONSTRAINT individual_fk FOREIGN KEY (individual_id)
REFERENCES mongoose.individual (individual_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: litter_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.pregnancy DROP CONSTRAINT IF EXISTS litter_fk CASCADE;
ALTER TABLE mongoose.pregnancy ADD CONSTRAINT litter_fk FOREIGN KEY (litter_id)
REFERENCES mongoose.litter (litter_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: mongoose.weight | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.weight CASCADE;
CREATE TABLE mongoose.weight(
	weight_id serial NOT NULL,
	individual_id integer,
	weight integer NOT NULL,
	"time" timestamp NOT NULL,
	accuracy smallint,
	session text,
	collar_weight integer,
	location geography(POINT, 4326),
	comment text,
	CONSTRAINT weight_pk PRIMARY KEY (weight_id)

);
-- ddl-end --
ALTER TABLE mongoose.weight OWNER TO postgres;
-- ddl-end --

-- object: individual_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.weight DROP CONSTRAINT IF EXISTS individual_fk CASCADE;
ALTER TABLE mongoose.weight ADD CONSTRAINT individual_fk FOREIGN KEY (individual_id)
REFERENCES mongoose.individual (individual_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: mongoose.ultrasound | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.ultrasound CASCADE;
CREATE TABLE mongoose.ultrasound(
	ultrasound_id serial NOT NULL,
	individual_id integer,
	observation_date timestamp NOT NULL,
	foetus_number smallint,
	foetus_size text,
	cross_view_length decimal,
	cross_view_width decimal,
	long_view_length decimal,
	long_view_width decimal,
	observer text,
	comment text,
	CONSTRAINT ultrasound_pk PRIMARY KEY (ultrasound_id)

);
-- ddl-end --
ALTER TABLE mongoose.ultrasound OWNER TO postgres;
-- ddl-end --

-- object: individual_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.ultrasound DROP CONSTRAINT IF EXISTS individual_fk CASCADE;
ALTER TABLE mongoose.ultrasound ADD CONSTRAINT individual_fk FOREIGN KEY (individual_id)
REFERENCES mongoose.individual (individual_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: mongoose.radiocollar | type: TABLE --
-- DROP TABLE IF EXISTS mongoose.radiocollar CASCADE;
CREATE TABLE mongoose.radiocollar(
	radiocollar_id serial NOT NULL,
	individual_id integer,
	frequency smallint,
	weight smallint,
	fitted timestamp,
	turned_on timestamp,
	removed timestamp,
	comment text,
	date_entered timestamp,
	CONSTRAINT radiocollar_pk PRIMARY KEY (radiocollar_id)

);
-- ddl-end --
ALTER TABLE mongoose.radiocollar OWNER TO postgres;
-- ddl-end --

-- object: individual_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.radiocollar DROP CONSTRAINT IF EXISTS individual_fk CASCADE;
ALTER TABLE mongoose.radiocollar ADD CONSTRAINT individual_fk FOREIGN KEY (individual_id)
REFERENCES mongoose.individual (individual_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: pack_event_code_fk | type: CONSTRAINT --
-- ALTER TABLE mongoose.pack_event DROP CONSTRAINT IF EXISTS pack_event_code_fk CASCADE;
ALTER TABLE mongoose.pack_event ADD CONSTRAINT pack_event_code_fk FOREIGN KEY (pack_event_code_id)
REFERENCES mongoose.pack_event_code (pack_event_code_id) MATCH FULL
ON DELETE SET NULL ON UPDATE CASCADE;
-- ddl-end --

-- object: pack_secondary | type: CONSTRAINT --
-- ALTER TABLE mongoose.interaction DROP CONSTRAINT IF EXISTS pack_secondary CASCADE;
ALTER TABLE mongoose.interaction ADD CONSTRAINT pack_secondary FOREIGN KEY (secondpack_id)
REFERENCES mongoose.pack (pack_id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --



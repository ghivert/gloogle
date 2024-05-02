SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: moddatetime; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS moddatetime WITH SCHEMA public;


--
-- Name: EXTENSION moddatetime; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION moddatetime IS 'functions for tracking last modification time';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: type_nature; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.type_nature AS ENUM (
    'function',
    'type_definition',
    'type_alias',
    'constant'
);


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: hex_read; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.hex_read (
    id integer NOT NULL,
    last_check timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: hex_read_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.hex_read ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.hex_read_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: hex_user; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.hex_user (
    id integer NOT NULL,
    username text NOT NULL,
    email text,
    url text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: hex_user_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.hex_user ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.hex_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: package; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.package (
    id integer NOT NULL,
    name text NOT NULL,
    repository text,
    documentation text,
    hex_url text,
    links jsonb,
    licenses jsonb,
    description text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: package_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.package ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.package_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: package_owner; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.package_owner (
    hex_user_id integer NOT NULL,
    package_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: package_release; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.package_release (
    id integer NOT NULL,
    package_id integer,
    version text NOT NULL,
    url text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: package_release_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.package_release ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.package_release_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: package_type_fun_signature; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.package_type_fun_signature (
    id integer NOT NULL,
    name text NOT NULL,
    documentation text NOT NULL,
    signature_ text NOT NULL,
    nature public.type_nature NOT NULL,
    parameters integer[] NOT NULL,
    metadata jsonb NOT NULL,
    module text NOT NULL,
    package_release_id integer,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: package_type_fun_signature_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.package_type_fun_signature ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.package_type_fun_signature_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying(128) NOT NULL
);


--
-- Name: hex_read hex_read_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.hex_read
    ADD CONSTRAINT hex_read_pkey PRIMARY KEY (id);


--
-- Name: hex_user hex_user_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.hex_user
    ADD CONSTRAINT hex_user_pkey PRIMARY KEY (id);


--
-- Name: hex_user hex_user_username_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.hex_user
    ADD CONSTRAINT hex_user_username_key UNIQUE (username);


--
-- Name: package package_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package
    ADD CONSTRAINT package_name_key UNIQUE (name);


--
-- Name: package_owner package_owner_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_owner
    ADD CONSTRAINT package_owner_pkey PRIMARY KEY (hex_user_id, package_id);


--
-- Name: package package_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package
    ADD CONSTRAINT package_pkey PRIMARY KEY (id);


--
-- Name: package_release package_release_package_id_version_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_release
    ADD CONSTRAINT package_release_package_id_version_key UNIQUE (package_id, version);


--
-- Name: package_release package_release_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_release
    ADD CONSTRAINT package_release_pkey PRIMARY KEY (id);


--
-- Name: package_type_fun_signature package_type_fun_signature_package_release_id_module_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_type_fun_signature
    ADD CONSTRAINT package_type_fun_signature_package_release_id_module_name_key UNIQUE (package_release_id, module, name);


--
-- Name: package_type_fun_signature package_type_fun_signature_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_type_fun_signature
    ADD CONSTRAINT package_type_fun_signature_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: hex_user hex_user_moddatetime; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER hex_user_moddatetime BEFORE UPDATE ON public.hex_user FOR EACH ROW EXECUTE FUNCTION public.moddatetime('updated_at');


--
-- Name: package package_moddatetime; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER package_moddatetime BEFORE UPDATE ON public.package FOR EACH ROW EXECUTE FUNCTION public.moddatetime('updated_at');


--
-- Name: package_owner package_owner_moddatetime; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER package_owner_moddatetime BEFORE UPDATE ON public.package_owner FOR EACH ROW EXECUTE FUNCTION public.moddatetime('updated_at');


--
-- Name: package_release package_release_moddatetime; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER package_release_moddatetime BEFORE UPDATE ON public.package_release FOR EACH ROW EXECUTE FUNCTION public.moddatetime('updated_at');


--
-- Name: package_type_fun_signature package_type_fun_signature_moddatetime; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER package_type_fun_signature_moddatetime BEFORE UPDATE ON public.package_type_fun_signature FOR EACH ROW EXECUTE FUNCTION public.moddatetime('updated_at');


--
-- Name: package_owner package_owner_hex_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_owner
    ADD CONSTRAINT package_owner_hex_user_id_fkey FOREIGN KEY (hex_user_id) REFERENCES public.hex_user(id);


--
-- Name: package_owner package_owner_package_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_owner
    ADD CONSTRAINT package_owner_package_id_fkey FOREIGN KEY (package_id) REFERENCES public.package(id);


--
-- Name: package_release package_release_package_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_release
    ADD CONSTRAINT package_release_package_id_fkey FOREIGN KEY (package_id) REFERENCES public.package(id);


--
-- Name: package_type_fun_signature package_type_fun_signature_package_release_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_type_fun_signature
    ADD CONSTRAINT package_type_fun_signature_package_release_id_fkey FOREIGN KEY (package_release_id) REFERENCES public.package_release(id);


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO public.schema_migrations (version) VALUES
    ('20240412153209'),
    ('20240412154006'),
    ('20240412154007'),
    ('20240412154008'),
    ('20240412154430'),
    ('20240412155057'),
    ('20240413164020');

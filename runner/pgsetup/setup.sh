#!/bin/bash
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
create table if not exists public.browsers
(
    id   serial
        primary key,
    name varchar(255) not null
        unique
);

alter table public.browsers
    owner to mxssy;

insert into browsers (id, name) VALUES (1, 'chromium'), (2, 'webkit'), (3, 'firefox');

create table if not exists public.modes
(
    id   serial
        primary key,
    name varchar(255) not null
        unique
);

alter table public.modes
    owner to mxssy;
insert into modes (id, name) VALUES (1, 'innerHTML'), (2, 'document_write');
create table if not exists public.sanitizers
(
    id   serial
        primary key,
    name varchar(255) not null
        unique
);

alter table public.sanitizers
    owner to mxssy;

create table if not exists public.rng_state
(
    id        serial
        constraint pk_rng_state
            primary key,
    state     text                                   not null,
    timestamp timestamp with time zone default now() not null
);

alter table public.rng_state
    owner to mxssy;

create table if not exists public.generations
(
    id           bigserial
        primary key,
    hash         bigint                                 not null
        unique,
    payload      text                                   not null,
    pretty       text                                   not null,
    json         text                                   not null,
    timestamp    timestamp with time zone default now() not null,
    rng_state_id bigint                                 not null
        constraint fk_generation_rng
            references public.rng_state
);

alter table public.generations
    owner to mxssy;

create table if not exists public.sanitized
(
    id            bigserial
        primary key,
    gen_id        bigint                                 not null
        constraint fk_generation
            references public.generations,
    sanitizer_id  integer                                not null
        constraint fk_sanitizer
            references public.sanitizers,
    output        text,
    serialized    text,
    errored       integer                  default 0     not null,
    error_message text,
    done          integer                  default 0     not null,
    timestamp     timestamp with time zone default now() not null,
    unique (gen_id, sanitizer_id)
);

alter table public.sanitized
    owner to mxssy;

create index if not exists sanitized_generation
    on public.sanitized (gen_id);

create index if not exists sanitized_done
    on public.sanitized (done);

create index if not exists sanitized_gen_id
    on public.sanitized (gen_id);

create index if not exists sanitized_sanitizer
    on public.sanitized (sanitizer_id);

create table if not exists public.to_sanitize
(
    id           bigserial
        primary key,
    gen_id       bigint not null
        constraint fk_to_sanitize_gen
            references public.generations,
    sanitizer_id int not null
        constraint fk_to_sanitize_sanitizer
            references public.sanitizers,
    sanitized_id int
        constraint fk_to_sanitize_sanitized
            references public.sanitized,
    unique (gen_id, sanitizer_id)
);

alter table public.to_sanitize
    owner to mxssy;

create index if not exists to_sanitize_todos
    on public.to_sanitize (sanitizer_id, sanitized_id);

create index if not exists idx_sanitized_todo
    on public.to_sanitize (sanitized_id);

create index if not exists idx_to_sanitize_sanitizer
    on public.to_sanitize (sanitizer_id);

create table if not exists public.evaluations
(
    id            bigserial
        primary key,
    gen_id        bigint  not null
        constraint fk_generation
            references public.generations,
    sanitized_id  bigint
        constraint fk_sanitized
            references public.sanitized,
    browser_id    integer
        constraint fk_eval_browser
            references public.browsers,
    sanitizer_id  integer
        constraint fk_eval_sanitizer
            references public.sanitizers,
    mode_id       integer
        constraint fk_eval_mode
            references public.modes,
    executed      integer not null,
    result        text,
    serialized    text,
    error_message text,
    status        integer not null,
    timestamp     timestamp with time zone default now() not null,
    constraint evaluations_gen_id_sanitized_id_browser_id_sanitizer_id_mod_key
        unique (gen_id, sanitized_id, browser_id, sanitizer_id, mode_id)
);

alter table public.evaluations
    owner to mxssy;

create index if not exists eval_generation
    on public.evaluations (gen_id);

create index if not exists eval_executed
    on public.evaluations (executed);

create index if not exists eval_gen_id
    on public.evaluations (gen_id);

create index if not exists eval_sanitized_id
    on public.evaluations (sanitized_id);

create index if not exists eval_browser
    on public.evaluations (browser_id);

create index if not exists eval_sanitizer
    on public.evaluations (sanitizer_id);

create index if not exists eval_mode
    on public.evaluations (mode_id);

create index if not exists diverging_idx
    on public.evaluations (gen_id, sanitizer_id, sanitized_id, mode_id);

create index if not exists sanitized_mode
    on public.evaluations (sanitized_id, mode_id);

create table if not exists public.to_evaluate
(
    id           bigserial
        primary key,
    sanitized_id bigint
        constraint fk_to_sanitize_sanitized
            references public.sanitized,
    browser_id   bigint not null
        constraint fk_to_evaluate_browser
            references public.browsers,
    mode_id      bigint not null
        constraint fk_to_evaluate_mode
            references public.sanitizers,
    eval_id      bigint
        constraint fk_to_evaluate_eval
            references public.evaluations,
    unique (sanitized_id, browser_id, mode_id, eval_id)
);

alter table public.to_evaluate
    owner to mxssy;

create index if not exists to_evaluate_todos
    on public.to_evaluate (browser_id, eval_id);

create index if not exists idx_eval_todo
    on public.to_evaluate (eval_id);


create or replace procedure public.insert_generation(IN hash bigint, IN payload text, IN pretty text, IN json text, IN rng_state_id bigint)
    language plpgsql
as
\$\$
DECLARE
    sanitizer RECORD;
    gen_id bigint;
BEGIN
    INSERT INTO generations (hash, payload, pretty, json, rng_state_id) VALUES (hash, payload, pretty, json, rng_state_id) returning id into gen_id;
    FOR sanitizer IN SELECT * FROM sanitizers LOOP
        insert into to_sanitize (gen_id, sanitizer_id) values(gen_id, sanitizer.id);
    END LOOP;
    COMMIT;
END;
\$\$;

alter procedure public.insert_generation(bigint, text, text, text, bigint) owner to mxssy;

create or replace procedure public.insert_sanitized(IN gen_id bigint, IN sanitizer_id bigint, IN serialized text, IN output text, IN errored integer, IN error_message text, IN done integer, IN tsid bigint)
    language plpgsql
as
\$\$
DECLARE
    browser RECORD;
    mode RECORD;
    san_id bigint;
BEGIN
    INSERT INTO sanitized (gen_id, sanitizer_id, serialized, output, errored, error_message, done) VALUES (gen_id, sanitizer_id, serialized, output, errored, error_message, done) returning id into san_id;
    UPDATE to_sanitize set sanitized_id = san_id where id = tsid and sanitized_id is null;
    if done = 0 then
    FOR browser IN SELECT * FROM browsers LOOP
            FOR mode IN SELECT * FROM modes LOOP
                    insert into to_evaluate(sanitized_id, browser_id, mode_id) values(san_id, browser.id, mode.id);
                END LOOP;
        END LOOP;
    end if;
END;
\$\$;

alter procedure public.insert_sanitized(bigint, bigint, text, text, integer, text, integer, bigint) owner to mxssy;

create or replace procedure public.insert_sanitizer(IN sanitizer_name text)
    language plpgsql
as
\$\$
DECLARE
    gen RECORD;
    sanitizer_id bigint;
BEGIN
    INSERT INTO sanitizers (name) VALUES (sanitizer_name) returning id into sanitizer_id;
    FOR gen IN SELECT id FROM generations LOOP
            insert into to_sanitize (gen_id, sanitizer_id) values(gen.id, sanitizer_id);
        END LOOP;
END;
\$\$;

alter procedure public.insert_sanitizer(text) owner to mxssy;

create or replace procedure public.add_done_to_eval()
    language plpgsql
as
\$\$
DECLARE
    browser RECORD;
    sanitized RECORD;
BEGIN


        FOR browser IN SELECT * FROM browsers LOOP
                    FOR sanitized IN SELECT * FROM public.sanitized where done = 1 LOOP
                        insert into to_evaluate(sanitized_id, browser_id, mode_id) values(sanitized.id, browser.id, 1);
                        insert into to_evaluate(sanitized_id, browser_id, mode_id) values(sanitized.id, browser.id, 2);

        END LOOP;
    end loop;
END;
\$\$;

alter procedure public.add_done_to_eval() owner to mxssy;

insert into sanitizers (id, name) values
                                      (1, 'no-sanitizer'),
                                      (2, 'caja (node)'),
(3, 'caja2 (node)'),
(4, 'dompurify (node)'),
(5, 'dompurify (lax, node)'),
(6, 'dompurify (current, node22)'),
(7, 'dompurify (lax, current, forceBody, node22)'),
(8, 'sanitize-html (node)'),
(9, 'sanitize-html (curr, node)'),
(10, 'typo3'),
(11, 'typo3-lax'),
(12, 'ganss'),
(13, 'ganss-lax'),
(14, 'html-rule'),
(15, 'html-rule-lax'),
(16, 'typo3-fix'),
(17, 'typo3-fix-lax'),
(18, 'ganss-fix'),
(19, 'ganss-fix-lax'),
(20, 'typo3-fix2'),
(21, 'typo3-fix2-lax'),
(22, 'ruby-sanitize'),
(23, 'ruby-sanitize-lax'),
(24, 'loofah-prune'),
(25, 'antisamy'),
(26, 'antisamy-anythinggoes'),
(27, 'antisamy-lax'),
(28, 'jsoup'),
(29, 'jsoup-lax')

EOSQL

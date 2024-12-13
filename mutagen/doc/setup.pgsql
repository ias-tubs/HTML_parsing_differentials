create table browsers
(
    id   serial
        primary key,
    name varchar(255) not null
        unique
);

alter table browsers
    owner to mxssy;

create table sanitizers
(
    id   serial
        primary key,
    name varchar(255) not null
        unique
);

alter table sanitizers
    owner to mxssy;


create table if not exists generations
(
    id      serial
        primary key,
    hash    bigint not null
        unique,
    payload text   not null,
    pretty  text   not null,
    json    text   not null,
    timestamp     timestamp with time zone default now() not null
);

alter table generations
    owner to mxssy;

create table if not exists evaluations
(
    id            serial
        primary key,
    gen_id        bigint                                 not null
        constraint fk_generation
            references generations,
    browser_id    integer                                not null,
    sanitizer_id  integer                                not null,
    error_message text,
    executed      integer                                not null,
    sanitized     text,
    result        text,
    serialized    text,
    status        integer                                not null,
    timestamp     timestamp with time zone default now() not null
);

alter table evaluations
    owner to mxssy;

create view diverging_executions
            (gen_id, chrome_exec, webkit_executed, firefox_executed, total_executed, payload, pretty, json, sanitized,
             chrome_result, chrome_serialized, wk_result, wk_serialized, ff_result, ff_serialized)
as
SELECT ec.gen_id,
       ec.executed                             AS chrome_exec,
       ew.executed                             AS webkit_executed,
       ef.executed                             AS firefox_executed,
       ec.executed + ew.executed + ef.executed AS total_executed,
       g.payload,
       g.pretty,
       g.json,
       ec.sanitized,
       ec.result                               AS chrome_result,
       ec.serialized                           AS chrome_serialized,
       ew.result                               AS wk_result,
       ew.serialized                           AS wk_serialized,
       ef.result                               AS ff_result,
       ef.serialized                           AS ff_serialized
FROM evaluations ec
         JOIN evaluations ew ON ec.gen_id = ew.gen_id AND ec.sanitizer_id = ew.sanitizer_id
         JOIN evaluations ef ON ec.gen_id = ef.gen_id AND ec.sanitizer_id = ef.sanitizer_id
         JOIN generations g ON ec.gen_id = g.id
WHERE ec.browser_id = 1
  AND ew.browser_id = 2
  AND ef.browser_id = 3
  AND ec.sanitizer_id = 1
  AND ew.sanitizer_id = 1
  AND ef.sanitizer_id = 1
  AND (ec.executed + ew.executed + ef.executed) > 0
  AND (ec.executed + ew.executed + ef.executed) < 3;

alter table diverging_executions
    owner to mxssy;


create table if not exists sanitized
(
    id            serial
        primary key,
    gen_id        bigint                                 not null
        constraint fk_generation
            references generations,
    sanitizer_id  integer                                not null
        constraint fk_sanitizer
            references sanitizers,
    output        text,
    errored       integer                  default 0     not null,
    error_message text,
    done          integer                  default 0     not null,
    timestamp     timestamp with time zone default now() not null
);

alter table sanitized
    owner to mxssy;

create index if not exists sanitized_generation
    on sanitized (gen_id);
create index if not exists sanitized_done
    on sanitized (done);

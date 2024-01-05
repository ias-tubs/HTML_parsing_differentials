# Orchestration

This folder provides a docker-compose environment to run all this with minimal (tm) effort.

## Setup

After having built all the required docker containers in both the [MutaGen](../mutagen/) as well was the [testbed](../testbed/) folder, you probably should configure the provided environment for your needs.

1. Database

For the database there are several settings you (might) need to adjust to your environment:

- Storage location

This project expects the presence of a sufficiently large and fast (i.e., SSD) storage volume for persistent storage of the database. 
You can choose an arbitrary (hopefully empty) folder to this end. The default value is: `/mnt/fastScratch/mxss_data/`.
You should change the location in the database entry of the compose file.
Please note, that depending on the number of payloads you want to generate and evaluate, the amount of data produced is fairly large (The dataset described in the paper was roughly 2 TB large)

- Configuration

Due to the large amount of data, the database is prone to become really slow to do joins over multiple tables. 
To keep any resemblance of performance, it is therefore quite memory hungry.
If your system has less than `512GB` RAM, I'd suggest adjusting the [pg.conf](pg.conf) to reduce memory pressure.

The same goes for number of processes, depending on the system.

The configuration was 'tuned' for a AMD EYPC system with *64 cores* (i.e., *128 threads*) and *512GB RAM*.

2. Number of browser processes

To speed up the process, you can scale up the number of concurrently running browser processed.
This is done by adjusting the value of `deploy.replicas` for the services `chrome-runner`, `webkit-runner`, and `firefox-runner` respectively.
As each runner starts a docker container with a full browser instance, slowly raise the number to not exhaust your systems resources.

3. Used sanitizers

Lastly, the compose file has an entry for each sanitizer we ran during our study. If you want to avoid testing specific ones, remove the entry or set `deploy.replicas` to zero.

## Usage

- Start: `docker compose -f sanitizer_with_dom_export.yml up -d`
- Observe: `docker compose -f sanitizer_with_dom_export.yml logs -f`
- Stop: `docker compose -f sanitizer_with_dom_export.yml down`

## Analyzing the results

When running this project, the database is exposed to localhost:5432. You can use a suitable SQL client to start retrieving and analyzing data.
Here, it might be helpful to *only* start the database to keep system load during complex queries in check. To this end, stop the project as outlined above and run: `docker compose -f sanitizer_with_dom_export.yml up database -d`.

### Useful queries

Some useful SQL queries to select data and become familiar with the schema.

- How many samples were executed per sanitizer (i.e., which sanitizers are vulnerable?)

```sql
select s.name, count(distinct e.gen_id) from evaluations e join sanitizers s on s.id = e.sanitizer_id where executed = 1 group by s.name;
```

- Select samples bypassing a specific sanitizer

```sql
select g.id, s2.name, b.name, m.name, g.payload, s.output, s.serialized, e.result, e.serialized from evaluations e join modes m on e.mode_id = m.id join sanitized s on e.sanitized_id = s.id join sanitizers s2 on s2.id = s.sanitizer_id join generations g on e.gen_id = g.id join browsers b on e.browser_id = b.id where e.executed = 1 and s2.name = 'typo3' order by e.gen_id;

```
- Progress of the sanitizer/evaluation queues

The system uses two tables to "queue" samples for further evaluation. For each generated sample, MutaGen inserts an entry for every sanitizer known to the system into the `to_sanitize` table.
Similarly, after sanitizing a payload, the sanitizer does the same for each pair of browser and parsing mode for the `to_evaluate` table.

```sql
select 'sanitize' as kind, 'done' as name, count(*) as count from to_sanitize where sanitized_id is not null union
select 'sanitize' as kind, 'open' as name, count(*) as count from to_sanitize where sanitized_id is  null union
select 'eval' as kind, 'done' as name, count(*) as count from to_evaluate where to_evaluate.eval_id is not null union
select 'eval' as kind, 'open' as name, count(*) as count from to_evaluate where to_evaluate.eval_id is null order by kind, name;
```

This queries how many open and finished entries are in both queues. The project runs a background process to continuously remove finished items from both queues, to avoid having them clog up the queue.

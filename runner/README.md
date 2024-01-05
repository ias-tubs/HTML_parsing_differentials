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

## Environment

This starts a postgres database which expects to sta

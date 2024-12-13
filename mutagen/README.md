# MutaGen - All your mutations are belong to us

This repository contains MutaGen, an HTML fragment that builds HTML fragments to test against.
Additionally, it contains some hopefully helpful analysis tools to make sense of it all.
## Try it

1. Set up OCaml environment (see [Dockerfile](docker/Dockerfile.generator) for help)
2. Run it locally with `dune exec local`
3. Run the real thing

## Use it

1. Ensure database is running
2. Adjust `config.toml` to fit your needs or pass matching config via the `--config` flag.
3. Build the environment: `make build`, see above for setup hints



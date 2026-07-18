# Contributing to Cecelia Pineapple

Thanks for your interest. Cecelia Pineapple is early software and is primarily
maintained by its author, [Dominik Schienstock](https://orcid.org/0000-0001-8440-0009).
Contributions — bug reports, fixes, and ideas — are welcome. Please read this
first.

## Before you start

- **Questions / usage help** → check the [FAQ](FAQ.md) first.
- **Bugs and features** → use the [issue templates](https://github.com/schienstockd/cecelia/issues/new/choose).
- **Larger changes** → open an issue to discuss the design *before* writing code,
  so effort isn't wasted on something that doesn't fit the architecture.

## Architecture is non-negotiable

The layer boundaries are deliberate and enforced: analysis logic lives only in
the headless `Cecelia.jl` package — never in the API or the Vue frontend — and
there is no fourth language. Read [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md)
and [`CLAUDE.md`](CLAUDE.md) before proposing structural changes. Departures from
the ported R behaviour should be deliberate and documented, not incidental.

## Development workflow

The full workflow — branching, commits, PRs, and releases — is documented in
[`docs/DEV.md`](docs/DEV.md). In short:

- Branch off the latest `main` with a conventional prefix (`feat/`, `fix/`,
  `docs/`, `chore/`, `refactor/`). **Never commit or push to `main` directly** —
  everything lands via a pull request.
- Keep a branch scoped to one logical change.
- **Every change ships with tests and docs.** The package must stay verifiable
  headlessly from the Julia REPL, without the GUI.

## Running the tests

Tests run per layer via Pixi (all of these run in CI):

```sh
pixi run test-pkg       # Cecelia.jl package (headless)
pixi run test-api       # Julia API adapters
pixi run test-frontend  # frontend logic (Vitest)
pixi run test-py        # Python analysis env
pixi run test-mcp       # MCP observer client
```

See [`docs/INSTALL.md`](docs/INSTALL.md) for the developer environment setup.

## A note on AI-assisted development

This project was built almost entirely with an AI assistant under human
direction — see [*How this software was built*](README.md#how-this-software-was-built).
That transparency expectation applies to contributions too: AI-assisted PRs are
fine, but you are responsible for the correctness of what you submit, and
scientific/visual claims must be validated by a human, not asserted by a model.

## License

By contributing, you agree that your contributions are licensed under
[GPL-3.0-or-later](LICENSE), the license of this project.

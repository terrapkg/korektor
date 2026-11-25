# korektor

korektor lints RPM spec files and serves the same findings over the language
server protocol. the parsing, evaluation and rules run through
[rozbir](https://github.com/terrapkg/rozbir); korektor adds config, output
formats, the CLI, and the language server.

коректор is Ukrainian for proofreader.

## usage

```sh
korektor lint                         # every *.spec under .
korektor lint foo.spec --target f44 --target el10
korektor lint --ruleset terra --format github
korektor lint --fix                   # apply safe fixes in place
korektor rules                        # list every rule (--markdown for docs/rules.md)
korektor lsp                          # language server on stdio
```

the same engine answers questions and makes edits without rpmspec, so it
works for scripts and update automation too:

```sh
korektor query foo.spec --target f44 --json    # name, EVR, sources, bconds, BuildRequires
korektor eval foo.spec '%{version} %{_libdir}' --target f44 --system-macros
korektor edit foo.spec --set-version 1.3 --changelog "update to 1.3"
korektor edit foo.spec --bump-release --changelog "rebuild"
korektor edit foo.spec --set-macro commit=abc123 --dry-run
```

`edit` follows `Version: %{ver}` into the `%global`, resets Release on a new
Version, and takes the changelog author from `--author` or the Packager tag.

## install

from Terra with `dnf5 install korektor`, or from source with
`cabal install exe:korektor`. a source build takes rozbir from Hackage, or
from `../rozbir` when that checkout exists.

## CI

as a GitHub Action, in a container that has Terra's repos. hand it the
output of `anda ci` so it lints only the projects that changed:

```yaml
jobs:
  manifest:
    outputs:
      build_matrix: ${{ steps.anda.outputs.build_matrix }}
    steps:
      - id: anda
        run: anda ci >> "$GITHUB_OUTPUT"
  lint:
    needs: manifest
    steps:
      - uses: actions/checkout@v7
      - uses: terrapkg/korektor@main
        with:
          anda-ci: ${{ needs.manifest.outputs.build_matrix }}
          args: --ruleset terra
```

`anda-ci` takes either the `build_matrix=[...]` line or the bare JSON. an
empty matrix lints nothing. without it, the action lints `paths` (default
`.`). the same works locally:

```sh
anda ci | korektor lint --anda-ci - --ruleset terra
```

and as a [pre-commit](https://pre-commit.com) hook, with the `korektor` id
from `.pre-commit-hooks.yaml`.

exit codes: 0 when clean, 1 when there are findings at or above `--fail-on`
(warning by default), 2 when korektor couldn't run.

## config

settings come from, weakest first: defaults, `korektor.toml` (found by walking
up from each spec to the repo root), `korektor_ignore` in the spec's
`anda.hcl` labels, `# rozbir: ignore[...]` comments in the spec, then flags.

```toml
rulesets = ["core", "rpmlint", "terra"]
fail-on = "warning"
system-macros = false
targets = ["f44", "el10", "um"]

[rules]
hardcoded-path = "off"
description-line-too-long = "info"

[target.um]
arch = "x86_64"
macros = { dist = ".um44", fedora = 44, rhel = false }
```

```hcl
project pkg {
  labels {
    korektor_ignore = "hardcoded-path, terra-packager"
  }
}
```

targets `fNN`, `elNN` and `rawhide` are built in, with an optional `:arch`
like `el10:aarch64`. without any targets, korektor evaluates for an unknown
target, so a condition like `%if 0%{?fedora}` is undecided and both branches
get linted. `system-macros = true` loads rpm's macro files from the machine,
which makes undefined macros really undefined, the way rpmbuild sees them.

## editors

- [Zed](editors/zed): `zed: install dev extension` on `editors/zed`. highlighting
  comes from [tree-sitter-rpmspec](https://gitlab.com/cryptomilk/tree-sitter-rpmspec)
- [JetBrains IDEs](editors/jetbrains) 2025.2 and newer, through the platform's
  LSP API. `gradle buildPlugin` builds the zip, fetching JDK 21 if needed
- anything else that speaks LSP: run `korektor lsp` for files ending in `.spec`

the server publishes diagnostics, offers each fix plus "ignore this rule on
this line" as quick fixes, shows what a macro expands to for every target on
hover, shows what each tag expands to as an inlay hint, jumps to
`%global`/`%define` lines and tags, finds references, renames a spec's own
macros, lists sections and subpackages as symbols, completes tags, sections
and macros, formats, and sends semantic tokens.

every rule is listed in [docs/rules.md](docs/rules.md).

## license

GPL-3.0-or-later. see [LICENSE.md](LICENSE.md).

## git version

- Change `ppx_driver` so that when `-diff-cmd -` is passed, they write the
  .corrected file but don't diff it or exit with a non-zero exit code.

  This is to make `[@@deriving_inline ...]` work with jbuilder. Jbuilder uses
  a separate build tree, so the current behavior of `ppx_driving` doesn't work
  well with jbuilder, especially the in-place behavior.

  What is done instead in jbuilder is that after running the ppx driver, it
  checks whether a .corrected file was created. If yes, jbuilder does the
  diffing itself, and by default also replaces the source file by the
  correction.'

## 113.43.00

- Update for the new context free API

## 113.24.00

- Disable safety check when code transformations are used as standard
  "-ppx" rewriters.

- Introduce reserved namespaces, see `Ppx_core`'s changelog.

  Pass errors as attribute with -dparsetree to avoid
  "Error while running external preprocessor".

- Update to follow `Ppx_core` evolution.

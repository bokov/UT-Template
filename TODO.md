## Future Design Notes for TidBits

### Workflow

1. `global.R` loads or installs and then loads `tidbits`
2. `global.R` establishes its own location using `here()`, a `tidbits` dependency
3. `global.R` looks for `config.R` and `local.config.R` one level above its own directory,
   and executes them in that order.
4. Sets default values for any required variables or options not specified by `config.R` or `local.config.R`
5. `global.R` looks for `functions.R` in `HERE/scripts` and in `HERE`, executing them in that order
6. `global.R` interprets the `.projpackages`, `.deps`, and `.debug` variables sent by the calling scriport
7. Loads `.projpackages`
8. Iterates over `.deps`, passing `.debug` and any other sticky options (e.g. formatting)

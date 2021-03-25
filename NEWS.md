# stantargets 0.0.0.9002

* Return the executable file after the Stan source file in model compilation targets.
* Replace the `log` argument with `stdout` and `stderr` (#23).
* Switch meaning of `%||%` and `%|||%` to conform to historical precedent.
* Add the `pedantic` argument for compilation (@sakrejda).

# stantargets 0.0.0.9001

* Join on data to summary output using `.join_data` in the Stan data (#18).
* Pre-compile models for testing and add an environment variable to skip tests that always force recompilation (#19).
* Load packages for any target computing summaries.

# stantargets 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

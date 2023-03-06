# Test System for py4cl2-cffi

`py4cl2-cffi` can be found at https://github.com/digikar99/py4cl2-cffi

# Running Tests

For running the tests, ensure you use python3.8 or recent (by configuring the exported variables in the package `py4cl2-cffi/config` in the file [src/config.lisp](https://github.com/digikar99/py4cl2-cffi/blob/master/src/config.lisp) and have the following installed on the python side:

- numpy
- networkx
- matplotlib

The system is configured (in [package.lisp](package.lisp)) to avoid testing the loading of networkx and matplotlib on ECL and ABCL. To overcome this behavior, simply remove `:ecl` or `:abcl` from `cl:*features*` during system-read time.

Once these are loaded, run `(asdf:test-system "py4cl2-tests")`.

# Differences with respect to py4cl2-tests

- **pythonize**: returns a `cffi:foreign-pointer` and not a string. However, `raw-pyeval` and `raw-pyexec` still expect strings. Thus, if `pythonize` is being used, then one expects to use `raw-pyeval` with `py4cl2` and `pyeval` with `py4cl2-cffi`.
- **+py-empty-tuple+** and **+py-none+**: `py4cl2-cffi` has explicit pointers to empty tuples and the None object as distinct from the empty list as well as the boolean False. `py4cl2` conflates these and uses the string `"()"` and `"None"` to denote the empty tuple and the None object.
- **pyvalue**: `py4cl2` relies on strings to point to the python objects and thus conflates when strings are being used to denote python objects vs when strings are used as literal objects. `py4cl2-cffi` uses `pyvalue` to avoid this conflation.
- Since arrays can be passed by reference, and no "excessive memory for eval-ing the array's string representation" is not required, pickling is no longer necessary.

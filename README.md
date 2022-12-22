# Test System for py4cl2-cffi

`py4cl2-cffi` can be found at https://github.com/digikar99/py4cl2-cffi

# Running Tests

For running the tests, ensure you use python3.8 or recent (by configuring the exported variables in the package `py4cl2-cffi/config` in the file [src/config.lisp](https://github.com/digikar99/py4cl2-cffi/blob/master/src/config.lisp) and have the following installed on the python side:

- numpy
- networkx
- matplotlib

The system is configured (in [package.lisp](package.lisp)) to avoid testing the loading of networkx and matplotlib on ECL and ABCL. To overcome this behavior, simply remove `:ecl` or `:abcl` from `cl:*features*` during system-read time.

Once these are loaded, run `(asdf:test-system "py4cl2-tests")`.

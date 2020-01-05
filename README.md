# tasty-rerun

This `Ingredient` for [`tasty`](https://hackage.haskell.org/package/tasty) testing framework
allows to filter a test tree depending
on an outcome of the previous run.
This may be useful in many scenarios,
especially when a test suite grows large.

For example, `tasty-rerun` allows:

* Rerun only tests, which failed during the last run (`--rerun`).
  Combined with live reloading (e. g., using `ghcid` or `stack test --file-watch`),
  it gives an ultimate power to focus on broken parts
  and put them back in shape, enjoying a tight feedback loop.
* Rerun only tests, which have beed added since the last saved test run.
  This comes handy when writing a new module, which does not affect other
  parts of the system, or adding new test cases.
* Rerun only tests, which passed during the last saved test run.
  Sometimes a part of the test suite is consistently failing
  (e. g., an external service is temporarily down), but you want be sure
  that you are not breaking anything else in course of your work.

To add it to your test suite just replace `Test.Tasty.defaultMain`
with `Test.Tasty.Ingredients.Rerun.defaultMainWithRerun`:

```haskell
import Test.Tasty
import Test.Tasty.Ingredients.Rerun

main :: IO ()
main = defaultMainWithRerun tests

tests :: TestTree
tests = undefined
```

Use `--help` to list command-line options:

* `--rerun`

  Rerun only tests, which failed during the last run.
  If the last run was successful, execute a full test
  suite afresh. A shortcut for `--rerun-update
  --rerun-filter failures,exceptions
  --rerun-all-on-success`.

* `--rerun-update`

  Update the log file to reflect latest test outcomes.

* `--rerun-filter CATEGORIES`

  Read the log file and rerun only tests from a given
  comma-separated list of categories: `failures`,
  `exceptions`, `new`, `successful`. If this option is
  omitted or the log file is missing, rerun everything.

* `--rerun-all-on-success`

  If according to the log file and `--rerun-filter` there
  is nothing left to rerun, run all tests. This comes
  especially handy in `stack test --file-watch` or
  `ghcid` scenarios.

* `--rerun-log-file FILE`

  Location of the log file (default: `.tasty-rerun-log`).

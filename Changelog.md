# 1.1.19

* Support tasty 1.5.

# 1.1.18

* Support tasty 1.4.

# 1.1.17

* Add `defaultMainWithRerun`,
  a drop-in replacement for `defaultMain`.

# 1.1.16

* New command-line option `--rerun-all-on-success`.
* New command-line shortcut `--rerun`.

# 1.1.15

* Bump upper bound of base.
* Restore missing -j command-line option.

# 1.1.14

* Support tasty 1.2.

# 1.1.13

* Bump upper bound of base.

# 1.1.12

* Bump upper bound of tasty.

# 1.1.11

* Bump upper bound of base.

# 1.1.10

* Bump upper bound of tasty.

# 1.1.9

* Bump upper bound of tasty.

# 1.1.8

* Bump upper bound of tasty.

# 1.1.7

* Allow base < 4.11.

# 1.1.6

* Allow base 4.9 for building with GHC 8.0

# 1.1.5

* Supports tasty < 0.12.

# 1.1.4

* Supports base <= 4.9, tagged <= 0.9

# 1.1.3

* Supports tasty =< 0.11

# 1.1.2

* Allow base 4.7 for building with GHC 7.8

# 1.1.1

* Update to work with tasty >= 0.8

# 1.1.0

* The `TestTree` is filtered using a custom traversal now, rather than a
  `TreeFold`. This gives better guarantees that the `TestTree` is only
  reduced and that nodes (such as `WithResources`) continue to work. The
  resulting filtered `TestTree` now has the same shape as the original
  tree, but filtered tests are transformed into `TestGroup`s with no tests.
  This is a fairly major change to how the filtering is performed, so this
  is a new major release, and previous versions are now considered
  deprecated.

# 1.0.1

* Now supports filtering `TestTree`s that use resources.

# 1.0.0

* Initial release. Supports the `--rerun-update`, `--rerun-log-file` and
  `--rerun-filter` options. Supported filters are `new`, `failures`,
  `exceptions` and `successful`.

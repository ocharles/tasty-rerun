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

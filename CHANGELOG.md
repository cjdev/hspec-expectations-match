# 0.2.0.0 (November 17th, 2017)

- Added `assertDo`, which automatically instruments a `do` block with uses of `shouldReturnAndMatch`.

# 0.1.1.0 (November 15th, 2017)

- Added `shouldReturnAndMatch`, which combines the action-running behavior of `shouldReturn` with the pattern-matching behavior of `shouldMatch`.
- Added a missing `HasCallStack` constraint to properly provide source location information for failed expectations.

# 0.1.0.0 (November 15th, 2017)

- Initial release.

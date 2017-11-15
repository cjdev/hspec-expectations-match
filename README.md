# hspec-expectations-match [![Build Status](https://travis-ci.org/cjdev/hspec-expectations-match.svg?branch=master)](https://travis-ci.org/cjdev/hspec-expectations-match)

`hspec-expectations-match` is a Haskell package that provides expectations for use with [`hspec`][hspec] that use Template Haskell to assert that a value matches a particular pattern. The primary expectation is `shouldMatch`:

```haskell
{-# LANGUAGE TemplateHaskell #-}

ghci> $([|Just True|] `shouldMatch` [p|Just _|])
ghci> $([|Nothing|] `shouldMatch` [p|Just _|])
*** Exception: Nothing failed to match pattern (Just x)
```

Additionally, any bindings created by the pattern will be returned if the pattern is successfully matched, making it easier to extract the result of some assertion and use it to make further assertions.

```haskell
ghci> $([|Just True|] `shouldMatch` [p|Just x|])
True
ghci> $([|[1, 2]|] `shouldMatch` [p|[x, y]|])
(1, 2)
```

For more information, [see the documentation on Hackage][hspec-expectations-match].

[hspec]: https://hackage.haskell.org/package/hspec
[hspec-expectations-match]: https://hackage.haskell.org/package/hspec-expectations-match

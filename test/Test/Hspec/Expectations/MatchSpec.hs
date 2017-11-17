{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Hspec.Expectations.MatchSpec (spec) where

import Test.Hspec
import Test.Hspec.Expectations.Match

spec :: Spec
spec = do
  describe "shouldMatch" $ do
    it "succeeds when a value matches a pattern" $ example $ do
      $([|Just True|] `shouldMatch` [p|Just True|])
      $([|Just True|] `shouldMatch` [p|Just _|])
      $([|Just False|] `shouldMatch` [p|Just _|])

      $([|17 :: Integer|] `shouldMatch` [p|((>= 15) -> True)|])

    it "fails when a value does not match a pattern" $ do
      $([|Just True|] `shouldMatch` [p|Nothing|]) `shouldThrow` anyException
      $([|Nothing|] `shouldMatch` [p|Just True|]) `shouldThrow` anyException

      $([|17 :: Integer|] `shouldMatch` [p|((<= 15) -> True)|]) `shouldThrow` anyException

    it "returns any bindings matched by a successful pattern" $ do
      a <- $([|Just True|] `shouldMatch` [p|Just x|])
      a `shouldBe` True

      (b, c) <- $([|['x', 'y']|] `shouldMatch` [p|[x, y]|])
      b `shouldBe` 'x'
      c `shouldBe` 'y'

  describe "shouldReturnAndMatch" $
    it "matches the result of an action against a pattern" $ do
      $([|pure (Just True)|] `shouldReturnAndMatch` [p|Just _|])

      $([|pure Nothing|] `shouldReturnAndMatch` [p|Just True|]) `shouldThrow` anyException

      a <- $([|pure (Just True)|] `shouldReturnAndMatch` [p|Just x|])
      a `shouldBe` True

  describe "assertDo" $
    it "automatically annotates pattern binds with shouldReturnAndMatch" $ do
      $(assertDo [|do
        () <- pure ()
        pure ()|])

      $(assertDo [|do
        Just x <- pure $ Just True
        x `shouldBe` True|])

      $(assertDo [|do
        Just x <- pure Nothing
        x `shouldBe` True|]) `shouldThrow` anyException

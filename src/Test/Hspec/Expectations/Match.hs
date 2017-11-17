{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
  This package provides expectations for use with @hspec@ that use Template
  Haskell to assert that a value matches a particular pattern. Furthermore,
  any bindings created by the pattern will be returned if the pattern is
  successfully matched, making it easier to extract the result of some assertion
  and use it to make further assertions.

  These functions should be used with Template Haskell’s expression and pattern
  quoters, notated by @[| ... |]@ and @[p| ... |]@, respectively.
-}
module Test.Hspec.Expectations.Match
  ( shouldMatch
  , shouldReturnAndMatch
  , assertDo
  ) where

import Control.Monad.Base (MonadBase, liftBase)
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Test.Hspec.Expectations (expectationFailure)

import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Syntax

assertPatternMatchFailure :: (HasCallStack, MonadBase IO m, Show a) => String -> a -> m b
assertPatternMatchFailure pat val =
    liftBase (expectationFailure (showsPrec 11 val "" ++ " failed to match pattern " ++ pat))
    -- expectationFailure should always throw, but it returns IO (), not IO a,
    -- so we need to make the typechecker happy by handling the impossible case.
    *> pure (error "assertPatternMatchFailure: internal error")

-- | Asserts that a value matches a given pattern and returns the pattern’s
-- bindings if the match succeeds.
--
-- >>> a <- $([|Just True|] `shouldMatch` [p|Just x|])
-- >>> a
-- True
-- >>> a <- $([|Nothing|] `shouldMatch` [p|Just x|])
-- *** Exception: Nothing failed to match pattern (Just x)
--
-- If multiple values are bound by a pattern, they are returned in a tuple,
-- in the order they appear in the pattern.
--
-- >>> (b, c) <- $([|['x', 'y']|] `shouldMatch` [p|[x, y]|])
-- >>> b
-- 'x'
-- >>> c
-- 'y'
-- >>> (b, c) <- $([|['x', 'y']|] `shouldMatch` [p|[x, y, z]|])
-- *** Exception: ['x','y'] failed to match pattern [x, y, z]
shouldMatch :: Q Exp -> Q Pat -> Q Exp
shouldMatch qExpr qPat = do
  expr <- qExpr
  pat <- qPat
  patStr <- showsPat 11 pat

  valName <- newName "val"

  let successExpr = VarE 'pure `AppE` patBindingsToTupleExp pat
  let failureExpr = VarE 'assertPatternMatchFailure `AppE` LitE (StringL (patStr "")) `AppE` VarE valName

  pure $ CaseE expr
    [ Match pat (NormalB successExpr) []
    , Match (VarP valName) (NormalB failureExpr) []
    ]

-- | Like 'Test.Hspec.Expectations.shouldReturn' combined with 'shouldMatch'.
-- Like 'Test.Hspec.Expectations.shouldReturn', the provided expression should
-- produce an action that, once run, produces a value. Like 'shouldMatch', the
-- resulting value will be matched against the provided pattern.
shouldReturnAndMatch :: Q Exp -> Q Pat -> Q Exp
shouldReturnAndMatch qExpr qPat = do
  expr <- qExpr
  pat <- qPat
  patStr <- showsPat 11 pat

  valName <- newName "val"

  let successExpr = VarE 'pure `AppE` patBindingsToTupleExp pat
  let failureExpr = VarE 'assertPatternMatchFailure `AppE` LitE (StringL (patStr "")) `AppE` VarE valName

  pure $ VarE '(>>=) `AppE` expr `AppE` LamE [VarP valName]
    (CaseE (VarE valName)
      [ Match pat (NormalB successExpr) []
      , Match WildP (NormalB failureExpr) []
      ])

patBindingsToTupleExp :: Pat -> Exp
patBindingsToTupleExp = TupE . map VarE . patBindings

patBindingsToTuplePat :: Pat -> Pat
patBindingsToTuplePat = TupP . map VarP . patBindings

patBindings :: Pat -> [Name]
patBindings (LitP _) = []
patBindings (VarP nm) = [nm]
patBindings (TupP pats) = concatMap patBindings pats
patBindings (UnboxedTupP pats) = concatMap patBindings pats
patBindings (ConP _ pats) = concatMap patBindings pats
patBindings (InfixP patA _ patB) = patBindings patA ++ patBindings patB
patBindings (UInfixP patA _ patB) = patBindings patA ++ patBindings patB
patBindings (ParensP pat) = patBindings pat
patBindings (TildeP pat) = patBindings pat
patBindings (BangP pat) = patBindings pat
patBindings (AsP nm pat) = nm : patBindings pat
patBindings WildP = []
patBindings (RecP _ fieldPats) = concatMap (patBindings . snd) fieldPats
patBindings (ListP pats) = concatMap patBindings pats
patBindings (SigP pat _) = patBindings pat
patBindings (ViewP _ pat) = patBindings pat
#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
patBindings (UnboxedSumP pat _ _) = patBindings pat
#endif

-- The pretty-printer provided by template-haskell always prints things with
-- qualified names, which isn’t very pleasant for users trying to make sense of
-- expectation failures. While it’s unfortunately impossible to do the
-- completely correct thing here (we don’t know when a users’ code uses
-- qualified names, since TH discards that information), it seems like a better
-- default to print unqualified names in expectation failure messages.
showsPat :: Int -> Pat -> Q ShowS
showsPat prec p = case p of
    LitP lit -> pure $ showString (showLit lit)
    VarP nm -> pure $ showString (nameBase nm)
    TupP [] -> pure $ showString "()"
    TupP pats -> do
      pats' <- traverse (showsPat 0) pats
      pure $ showChar '(' . foldr1 (\s r -> s . showString ", " . r) pats' . showChar ')'
    UnboxedTupP [] -> pure $ showString "(# #)"
    UnboxedTupP pats -> do
      pats' <- traverse (showsPat 0) pats
      pure $ showString "(# " . foldr1 (\s r -> s . showString ", " . r) pats' . showString " #)"
    ConP nm [] -> pure $ showString (nameBase nm)
    ConP nm pats -> do
      pats' <- traverse (showsPat 11) pats
      pure . showParen (prec > 10) $
        showString (nameBase nm) . showChar ' ' . foldr1 (\s r -> s . showChar ' ' . r) pats'
    InfixP patA nm patB -> showInfix patA nm patB
    UInfixP patA nm patB -> showInfix patA nm patB
    ParensP pat -> showParen True <$> showsPat 0 pat
    TildeP pat -> (showChar '~' .) <$> showsPat 11 pat
    BangP pat -> (showChar '!' .) <$> showsPat 11 pat
    AsP nm pat -> ((showString (nameBase nm) . showChar '@') .) <$> showsPat 11 pat
    WildP -> pure $ showChar '_'
    RecP nm [] -> pure $ showString (nameBase nm) . showString " {}"
    RecP nm fieldPats -> do
      fieldPats' <- showFieldPats fieldPats
      pure $ showString (nameBase nm) . showString " { " . fieldPats' . showString " }"
    ListP [] -> pure $ showString "[]"
    ListP pats -> do
      pats' <- traverse (showsPat 0) pats
      pure $ showChar '[' . foldr1 (\s r -> s . showString ", " . r) pats' . showChar ']'
    SigP pat ty -> do
      pat' <- showsPat 10 pat
      pure . showParen (prec > 0) $ pat' . showString " :: " . showsPrec 10 (ppr ty)
    ViewP expr pat -> do
      pat' <- showsPat 10 pat
      pure . showParen (prec > 0) $ showsPrec 10 (ppr expr) . showString " -> " . pat'
#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
    UnboxedSumP pat alt arity -> do
      pat' <- showsPat 0 pat
      pure $
        showString "(#" . showString (replicate (alt - 1) '|') . pat'
        . showString (replicate (arity - alt) '|') . showString "#)"
#endif
  where
    showInfix patA nm patB = do
      Fixity nmPrec _ <- fromMaybe defaultFixity <$> reifyFixity nm
      patA' <- showsPat (nmPrec + 1) patA
      patB' <- showsPat (nmPrec + 1) patB
      pure . showParen (prec > nmPrec) $
        patA' . showChar ' ' . showString (nameBase nm) . showChar ' ' . patB'

    showFieldPats fieldPats = do
      fieldPats' <- traverse showFieldPat fieldPats
      pure $ foldr1 (\s r -> s . showString ", " . r) fieldPats'
    showFieldPat (nm, pat) = ((showString (nameBase nm) . showString " = ") .) <$> showsPat 0 pat

showLit :: Lit -> String
showLit (CharL c) = show c
showLit (StringL s) = show s
showLit (IntegerL i) = show i
showLit (RationalL r) = show (fromRational r :: Double)
showLit (IntPrimL i) = show i ++ "#"
showLit (WordPrimL i) = show i ++ "##"
showLit (FloatPrimL r) = show (fromRational r :: Float) ++ "#"
showLit (DoublePrimL r) = show (fromRational r :: Double) ++ "##"
showLit (StringPrimL s) = show s ++ "#"
showLit (CharPrimL c) = show c ++ "#"

-- | Instruments a @do@ block by automatically inserting uses of
-- 'shouldReturnAndMatch' for monadic bindings. Specifically, the transformation
-- converts this:
--
-- @
-- \$('assertDo'
--   [|do [x, y] <- 'return' [1, 2]
--        x `'Test.Hspec.Expectations.shouldBe`` 1
--        y `'Test.Hspec.Expectations.shouldBe`` 2|])
-- @
--
-- ...into this:
--
-- @
-- do (x, y) <- $([|'return' [1, 2]|] `'shouldReturnAndMatch`` [p|[x, y]|])
--    x `'Test.Hspec.Expectations.shouldBe`` 1
--    y `'Test.Hspec.Expectations.shouldBe`` 2
-- @
--
-- This makes it much easier to read @do@ blocks that make significant use of
-- 'shouldReturnAndMatch'.
--
-- Note that this transformation /only/ applies to monadic bindings (not @let@
-- bindings), and it does not occur when the pattern on the left hand side
-- already fully covers all potential values (that is, when the pattern could
-- not possibly fail to match).
assertDo :: Q Exp -> Q Exp
assertDo qDoExp = qDoExp >>= \case
    DoE stmts -> DoE <$> traverse annotateStatement stmts
    _ -> fail "assertDo: expected a do block"
  where
    annotateStatement stmt@(BindS pat expr) = case pat of
      VarP _ -> pure stmt
      WildP -> pure stmt
      _ -> do
        hasOtherCases <- patternHasOtherCases pat
        if hasOtherCases
          then BindS (patBindingsToTuplePat pat) <$> shouldReturnAndMatch (pure expr) (pure pat)
          else pure stmt
    annotateStatement stmt = pure stmt

    patternHasOtherCases (LitP _) = pure True
    patternHasOtherCases (VarP _) = pure False
    patternHasOtherCases (TupP pats) = or <$> traverse patternHasOtherCases pats
    patternHasOtherCases (UnboxedTupP pats) = or <$> traverse patternHasOtherCases pats
    patternHasOtherCases (ConP nm pats) = do
      DataConI _ _ tyNm <- reify nm
      tyInfo <- reify tyNm
      conHasOtherCases <- case tyInfo of
        TyConI dec -> case dec of
          DataD _ _ _ _ cons _ -> pure (length cons > 1)
          NewtypeD{} -> pure False
          _ -> fail ("patternHasOtherCases: internal error; unexpected declaration in TyConI: " ++ show dec)
        _ -> fail ("patternHasOtherCases: internal error; unexpected Info in DataConI: " ++ show tyInfo)
      if conHasOtherCases
        then pure True
        else or <$> traverse patternHasOtherCases pats
    patternHasOtherCases (InfixP patA nm patB) = patternHasOtherCases (ConP nm [patA, patB])
    patternHasOtherCases (UInfixP patA nm patB) = patternHasOtherCases (ConP nm [patA, patB])
    patternHasOtherCases (ParensP pat) = patternHasOtherCases pat
    patternHasOtherCases (TildeP pat) = patternHasOtherCases pat
    patternHasOtherCases (BangP pat) = patternHasOtherCases pat
    patternHasOtherCases (AsP _ pat) = patternHasOtherCases pat
    patternHasOtherCases WildP = pure False
    patternHasOtherCases (RecP _ fieldPats) = or <$> traverse (patternHasOtherCases . snd) fieldPats
    patternHasOtherCases (ListP _) = pure True
    patternHasOtherCases (SigP pat _) = patternHasOtherCases pat
    patternHasOtherCases (ViewP _ _) = pure True

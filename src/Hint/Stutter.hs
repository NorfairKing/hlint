{-
    Find type occurrences whose one-component module qualifier repeats the type
    name, such as 'Map.Map', 'Set.Set' or 'Text.Text'. A module may be imported
    twice, so the qualified import can stay and a second import can bring
    just the type into scope unqualified:

    > import Data.Map (Map)
    > import Data.Map qualified as Map

    A qualifier that is a full module name, as in 'Data.Map.Map', is left alone.
    Two modules that both export a 'Map' can only be told apart by their full
    names, so there the qualifier is carrying its weight.

<TEST>
foo :: Map.Map k v -- @Suggestion Map
type Foo = Set.Set Int -- Set
data Foo = Foo Text.Text -- Text
newtype Foo = Foo {unFoo :: Seq.Seq Int} -- Seq
instance C Text.Text -- Text
foo = undefined :: Map.Map k v -- Map
import qualified Data.Map as Map \
foo :: Map.Map k v -- @Note requires importing Data.Map (Map)
import qualified Data.Map as M \
foo :: M.Map k v
foo :: Map k v
foo :: Map.Key k
foo :: Data.Map.Map k v
foo :: Data.Map.Strict.Map k v
import qualified Data.Map \
import qualified Foo.Map \
foo :: Data.Map.Map k v -> Foo.Map.Map k v
foo = Map.Map
foo = Map.empty
import Data.Map as Map
</TEST>
-}

module Hint.Stutter(stutterHint) where

import Hint.Type(DeclHint,Note(..),Severity(..),rawIdea)
import GHC.Util(Scope,possModules)

import Data.Generics.Uniplate.DataOnly
import Prelude

import GHC.Hs
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable

stutterHint :: DeclHint
stutterHint scope _ decl =
    [ rawIdea Suggestion "Redundant module qualifier" (locA l)
        (unsafePrettyPrint ty)
        (Just $ unsafePrettyPrint unqualified)
        -- Dropping the qualifier only compiles once the type is in scope
        -- unqualified, which HLint cannot arrange, so offer no refactoring.
        [Note $ importNote scope lname occ]
        []
    | L l ty@(HsTyVar x promoted lname@(L nameLoc name)) <- universeBi decl :: [LHsType GhcPs]
    , stutters name
    , let occ = rdrNameOcc name
    , let unqualified = HsTyVar x promoted (L nameLoc (mkRdrUnqual occ)) :: HsType GhcPs
    ]

-- | Is the qualifier a single component repeating the name it qualifies, as in
-- @Map.Map@? A qualifier that is a full module name, as in @Data.Map.Map@, is
-- how you tell two modules' @Map@s apart, so it does not count.
stutters :: RdrName -> Bool
stutters (Qual modu occ) = moduleNameString modu == occNameString occ
stutters _ = False

-- | Spell out the import that brings the type into scope unqualified whenever
-- the qualifier resolves to a single module. Deliberately silent on whether it
-- joins the qualified import or replaces it, since that depends on whether the
-- qualifier is used elsewhere in the module.
importNote :: Scope -> LocatedN RdrName -> OccName -> String
importNote scope name occ = case possModules scope name of
    [modu] -> "requires importing " ++ moduleNameString modu ++ " (" ++ occNameString occ ++ ")"
    _ -> "requires importing " ++ occNameString occ ++ " unqualified"

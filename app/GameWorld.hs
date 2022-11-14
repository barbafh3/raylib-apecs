-- {-# LANGUAGE TemplateHaskellQuotes #-}
module GameWorld where
-- import Language.Haskell.TH
-- import Apecs
-- import Apecs.TH
-- import Control.Monad (forM)

-- genName :: String -> Q Name
-- genName s = mkName . show <$> newName s

-- makeWorldFixedNoEC :: String -> [Name] -> Q [Dec]
-- makeWorldFixedNoEC worldName cTypes = do
--   cTypesNamesKinds <- forM cTypes $ \t -> do
--     rec <- genName "rec"
--     k <- reifyType t
--     return (ConT t, k, rec)

--   let wld = mkName worldName
--       has = mkName "Has"
--       sys = mkName "SystemT"
--       m = VarT $ mkName "m"
--       wldDecl = DataD [] wld [] Nothing [RecC wld records] []

--       makeRecord (t, k, n) = case k of
--         StarT -> (n, Bang NoSourceUnpackedness SourceStrict, ConT (mkName "Storage") `AppT` t)
--         AppT (AppT ArrowT StarT) StarT -> (n, Bang NoSourceUnpackedness SourceStrict, ConT (mkName "Storage") `AppT` (AppT t (ConT wld)))
--         _ -> error $ unwords ["Invalid component type for:", pprint t, "::", pprint k]
--       records = makeRecord <$> cTypesNamesKinds

--       makeInstance (t,k,n) = case k of
--         StarT ->
--           InstanceD Nothing [ConT (mkName "Monad") `AppT` m] (ConT has `AppT` ConT wld `AppT` m `AppT` t)
--             [ FunD (mkName "getStore") [Clause []
--                 (NormalB$ ConE sys `AppE` (VarE (mkName "asks") `AppE` VarE n))
--               [] ]
--             ]
--         AppT (AppT ArrowT StarT) StarT ->
--           InstanceD Nothing [ConT (mkName "Monad") `AppT` m] (ConT has `AppT` ConT wld `AppT` m `AppT` (AppT t (ConT wld)))
--             [ FunD (mkName "getStore") [Clause []
--                 (NormalB$ ConE sys `AppE` (VarE (mkName "asks") `AppE` VarE n))
--               [] ]
--             ]
--         _ -> error $ unwords ["Invalid component type for:", pprint t, "::", pprint k]
--       initWorldName = mkName $ "init" ++ worldName
--       initSig = SigD initWorldName (AppT (ConT (mkName "IO")) (ConT wld))
--       initDecl = FunD initWorldName [Clause []
--         (NormalB$ iterate (\wE -> AppE (AppE (VarE $ mkName "<*>") wE) (VarE $ mkName "explInit")) (AppE (VarE $ mkName "return") (ConE wld)) !! length records)
--         [] ]

--       hasDecl = makeInstance <$> cTypesNamesKinds

--   return $ wldDecl : initSig : initDecl : hasDecl

-- makeWorldFixed :: String -> [Name] -> Q [Dec]
-- makeWorldFixed worldName cTypes = makeWorldFixedNoEC worldName (cTypes ++ [''EntityCounter])

-- makeWorldAndComponentsFixed :: String -> [Name] -> Q [Dec]
-- makeWorldAndComponentsFixed worldName cTypes = do
--   wdecls <- makeWorldFixed worldName cTypes
--   cdecls <- makeMapComponents cTypes
--   return $ wdecls ++ cdecls

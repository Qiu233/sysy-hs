{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module SysY.AST.TH where


import Language.Haskell.TH
import Data.List (group, sort)

import SysY.AST.Basic
import Control.Monad (replicateM)
import Polysemy
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)


annotateDerivings :: [DerivClause]
annotateDerivings = [DerivClause Nothing [ConT ''Eq, ConT ''Show]]

makeAnnoTypes :: String -> Name -> [Name] -> Name -> Name -> Q [Dec]
makeAnnoTypes prefix anno_type_name annotated_ lift_class_name lift_func_name = do
    anno_type <- reify anno_type_name
    case anno_type of
        TyConI (DataD _ anno_name _ _ _ _) -> do
            concat <$> mapM (go anno_name) types
        _ -> fail "Annotation type must be normal ADT"
    where
        rmdups = map head . group . sort
        types = rmdups astTypes
        annotated = rmdups annotated_
        -- annotated_names = map (\t -> (t, mkName (prefix ++ nameBase t))) types
        prefixed n = mkName $ prefix ++ nameBase n
        go :: Name -> Name -> Q [Dec]
        go anno_name original = do
            let anno_ = original `elem` annotated
            o <- reify original
            case o of
                TyConI (DataD [] _ [] Nothing ctors _) -> do
                    -- liftIO $ print $ show derivings
                    -- for unknown reason, `derivings` is empty here, they have to be set manually
                    ctors_ <- mapM (goC anno_ anno_name) ctors
                    hom <- genHom anno_ original ctors
                    pure $ DataD [] (prefixed original) [] Nothing ctors_ annotateDerivings : catMaybes [hom]
                TyConI (NewtypeD [] _ [] Nothing ctor _) -> do
                    -- liftIO $ print $ show derivings
                    ctor_ <- goC anno_ anno_name ctor
                    hom <- genHom anno_ original [ctor]
                    pure $ NewtypeD [] (prefixed original) [] Nothing ctor_ annotateDerivings : catMaybes [hom]
                i -> fail $ "Only normal ADT is supported, found" ++ show i
        goC :: Bool -> Name -> Con -> Q Con
        goC anno_ anno_name (NormalC cname field_types) = do
            let anno_type = (Bang NoSourceUnpackedness NoSourceStrictness, ConT anno_name)
            field_types_ <- mapM goT field_types
            let field_types__ = if anno_ then anno_type : field_types_ else field_types_
            pure $ NormalC (prefixed cname) field_types__
        goC _ _ t = fail $ "Only normal constructor is supported, found " ++ show t
        goT :: BangType -> Q BangType
        goT (b, t) = (b, ) <$> goT' t
        goT' :: Type -> Q Type
        goT' (ConT n) = if n `elem` types
            then pure $ ConT $ prefixed n
            else pure $ ConT n
        goT' (AppT a b) = AppT a <$> goT' b
        goT' t = fail $ "Only concrete type is supported, found " ++ show t


        genHom :: Bool -> Name -> [Con] -> Q (Maybe Dec)
        genHom anno_ original ctors = do
            let type_ = foldl AppT (ConT lift_class_name) (map ConT [original, prefixed original])
            if anno_
                then pure Nothing
                else do
                    funcs <- genHomFuncs ctors
                    pure $ Just $ InstanceD Nothing [] type_ funcs
        genHomFuncs :: [Con] -> Q [Dec]
        genHomFuncs ctors = do
            a <- FunD lift_func_name <$> mapM genHomAnno ctors
            -- d <- FunD 'deanno <$> mapM genHomDeanno ctors
            pure [a]
        genHomAnno :: Con -> Q Clause
        genHomAnno (NormalC cname field_types) = do
            let name = prefixed cname
            field_names <- replicateM (length field_types) ((,) <$> newName "v" <*> newName "u")
            let pats = ConP cname [] (map (VarP . snd) field_names)
            let f (t, v, u) = do
                    t' <- goT' t
                    if t' == t
                        then pure $ LetS [ValD (VarP v) (NormalB (VarE u)) []]
                        else pure $ BindS (VarP v) (AppE (VarE lift_func_name) (VarE u))
            let l = zipWith (\(_, x) (y,z) -> (x, y, z)) field_types field_names
            stmts <- mapM f l
            let pure_stmt = NoBindS $ AppE (VarE 'pure) $
                    foldl AppE (ConE name) (map (VarE . fst) field_names)
            let doe = DoE Nothing (stmts ++ [pure_stmt])
            pure $ Clause [pats] (NormalB doe) []
        genHomAnno t = fail $ "Only normal constructor is supported, found " ++ show t

        -- genHomDeanno :: Con -> Q Clause
        -- genHomDeanno (NormalC cname field_types) = do
        --     let name = prefixed cname
        --     pats_names <- replicateM (length field_types) (newName "v")
        --     let pat = ConP name [] (map VarP pats_names)
        --     let body = AppE (VarE 'pure) $ foldl AppE (ConE cname) (map VarE pats_names)
        --     pure $ Clause [pat] (NormalB body) []
        -- genHomDeanno t = fail $ "Only normal constructor is supported, found " ++ show t

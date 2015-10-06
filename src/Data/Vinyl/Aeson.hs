{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vinyl.Aeson
  (RecJSON(..))
  where

import Data.Aeson
import Data.Aeson.Types (Parser(), Pair())
import Data.String (fromString)
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import GHC.TypeLits
import GHC.Exts

-- | A newtype wrapper around records with Symbol fields, which has a JSON instance
newtype RecJSON (f :: Symbol -> *) (fields :: [Symbol]) = RecJSON { _recJSON :: Rec f fields }

type family FieldsAll (fields :: [a]) (c :: a -> Constraint) where
  FieldsAll (field ': fields) c = (c field, FieldsAll fields c)
  FieldsAll '[] c = (() :: Constraint)

data ProxyDict (c :: a -> Constraint) (field :: a) where
  ProxyDict :: (c field) => proxy field -> ProxyDict c field

reifyFieldsConstraint :: (FieldsAll fields c) => proxy c -> Rec f fields -> Rec (ProxyDict c) fields
reifyFieldsConstraint constraintProxy (_ :& rest) = ProxyDict Proxy :& reifyFieldsConstraint constraintProxy rest
reifyFieldsConstraint _ RNil = RNil

reifyProxyConstraint :: (RecAll f fields c) => proxy c -> Rec g fields -> Rec (Compose (ProxyDict c) f) fields
reifyProxyConstraint constraintProxy (_ :& rest) = (Compose $ ProxyDict Proxy) :& reifyProxyConstraint constraintProxy rest
reifyProxyConstraint _ RNil = RNil

instance (RecApplicative fields, FieldsAll fields KnownSymbol, RecAll f fields FromJSON) => FromJSON (RecJSON f fields) where
  parseJSON =   withObject "A JSON-encoded record must be an Object" 
              $ fmap (fmap RecJSON)
              $ getCompose
              $ rtraverse (Compose . uncurry parseField . getLift :: forall field . Lift (,) (ProxyDict KnownSymbol) (Compose (ProxyDict FromJSON) f) field -> Compose ((->) Object) Parser (f field))
              $ rmap (\symbolDict -> Lift (\fieldDict -> Lift (symbolDict, fieldDict))) (reifyFieldsConstraint Proxy $ rpure $ Const ()) `rapply` (reifyProxyConstraint Proxy $ rpure $ Const ())

instance (FieldsAll fields KnownSymbol, RecAll f fields ToJSON) => ToJSON (RecJSON f fields) where
  toJSON (RecJSON record) = object $ recordToList $ rmap (\symbolDict -> Lift (\fieldDict -> Lift (\field -> fieldValue symbolDict fieldDict field))) (reifyFieldsConstraint Proxy record)
                                                    `rapply` (reifyConstraint Proxy record)
                                                    `rapply` record

parseField :: forall f field . ProxyDict KnownSymbol field -> Compose (ProxyDict FromJSON) f field -> Object -> Parser (f field)
parseField (ProxyDict fieldNameProxy) (Compose (ProxyDict _)) object = object .: (fromString $ symbolVal fieldNameProxy)

fieldValue :: forall f field . ProxyDict KnownSymbol field -> Compose (Dict ToJSON) f field -> f field -> Const Pair field
fieldValue (ProxyDict fieldNameProxy) (Compose (Dict _)) field = Const $ (fromString (symbolVal fieldNameProxy) .= toJSON field)


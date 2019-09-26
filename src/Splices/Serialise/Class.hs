{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE KindSignatures, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE BangPatterns, UndecidableInstances #-}
module Splices.Serialise.Class where

import Binary
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (ByteString)
import Data.Kind
import Data.Proxy
import Data.Word
import GHC.Generics
import GHC.TypeLits

import Splices.Conversions

-- * 'Serialise' class

class Serialise a where
  sput :: BinHandle -> a -> Conv ()
  default sput :: (Generic a, GSerialise (Rep a)) => BinHandle -> a -> Conv ()
  sput bh a = gput bh (from a)

  sget :: BinHandle -> Conv a
  default sget :: (Generic a, GSerialise (Rep a)) => BinHandle -> Conv a
  sget bh = to <$> gget bh

instance (Serialise a, Serialise b) => Serialise (a, b)
instance (Serialise a, Serialise b, Serialise c)
      => Serialise (a, b, c)

instance (Serialise a, Serialise b) => Serialise (Either a b)
instance Serialise a => Serialise (Maybe a)
instance Serialise Bool

instance Serialise a => Serialise [a] where
  sput bh xs = do
    sput bh (ViaBinary $ length xs)
    mapM_ (sput bh) xs

  sget bh = do
    ViaBinary (n :: Int) <- sget bh
    replicateM n (sget bh)

-- via Binary instance

newtype ViaBinary a = ViaBinary a

instance Binary a => Serialise (ViaBinary a) where
  sput bh (ViaBinary a) = liftRn $ liftIO (put_ bh a)
  sget bh = fmap ViaBinary . liftRn $ liftIO (get bh)

binsput :: Binary a => BinHandle -> a -> Conv ()
binsput bh a = sput bh (ViaBinary a)

binsget :: Binary a => BinHandle -> Conv a
binsget bh = do
  ViaBinary a <- sget bh
  return a

putTag :: BinHandle -> Word16 -> Conv ()
putTag bh w = sput bh (ViaBinary w)

getTag :: BinHandle -> Conv Word16
getTag bh = do
  ViaBinary w <- sget bh
  return w

instance Serialise Int where
  sput = binsput
  sget = binsget

instance Serialise Integer where
  sput = binsput
  sget = binsget

instance Serialise Char where
  sput = binsput
  sget = binsget

instance Serialise ByteString where
  sput = binsput
  sget = binsget

-- generic derivation of Serialise

class GSerialise f where
  gput :: BinHandle -> f p -> Conv ()
  gget :: BinHandle -> Conv (f p)

instance (GSerialise l, GSerialise r) => GSerialise (l :*: r) where
  gput bh (l :*: r) = gput bh l *> gput bh r
  gget bh = (:*:) <$> gget bh <*> gget bh

instance GSerialise f => GSerialise (M1 i c f) where
  gput bh (M1 f) = gput bh f
  gget bh = M1 <$> gget bh

instance Serialise a => GSerialise (K1 i a) where
  gput bh (K1 a) = sput bh a
  gget bh = K1 <$> sget bh

instance (GSerialiseSum l, GSerialiseSum r, SumSize l ~ nl, SumSize r ~ nr, KnownNat (nl+nr))
      => GSerialise (l :+: r) where
  gput bh lr = gputSum_ bh lr . fromIntegral $ natVal (Proxy @(nl+nr))
  gget bh = ggetSum_ bh . fromIntegral $ natVal (Proxy @(nl+nr))

instance GSerialise U1 where
  gput _ _ = pure ()
  gget _ = pure U1

instance GSerialise V1 where
  gput _ _ = pure ()
  gget _ = error "GSerialise.gget @V1"

-- utilities

type family SumSize (f :: k -> Type) :: Nat where
  SumSize (l :+: r)  = SumSize l + SumSize r
  SumSize (M1 C c a) = 1

type family SumSize' (f :: k -> Type) :: Nat where
  SumSize' (l :+: r) = SumSize l + SumSize r
  SumSize' (M1 C c a) = 1
  SumSize' (M1 D c a) = SumSize' a

gputSum_
  :: GSerialiseSum f => BinHandle -> f p -> Word16 -> Conv ()
gputSum_ bh a sz = gputSum bh a 0 sz

ggetSum_
  :: GSerialiseSum f => BinHandle -> Word16 -> Conv (f p)
ggetSum_ bh sz = do
  ViaBinary tag <- sget bh :: Conv (ViaBinary Word16)
  ggetSum bh tag sz

class GSerialiseSum f where
  gputSum :: BinHandle -> f p -> Word16 -> Word16 -> Conv ()
  ggetSum :: BinHandle -> Word16 -> Word16 -> Conv (f p)

instance GSerialise f => GSerialiseSum (M1 C c f) where
  gputSum bh f code _ = sput bh (ViaBinary code) >> gput bh f
  ggetSum bh _ _ = gget bh

instance (GSerialiseSum l, GSerialiseSum r) => GSerialiseSum (l :+: r) where
  gputSum bh lr !code !sz = case lr of
    L1 l -> gputSum bh l code sizeL
    R1 r -> gputSum bh r (code + sizeL) sizeR
    where sizeL = sz `shiftR` 1
          sizeR = sz - sizeL

  ggetSum bh !code !sz
    | code < sizeL = L1 <$> ggetSum bh code sizeL
    | otherwise    = R1 <$> ggetSum bh (code - sizeL) sizeR
    where sizeL = sz `shiftR` 1
          sizeR = sz - sizeL

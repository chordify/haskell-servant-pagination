{-# LANGUAGE TypeFamilies #-}

module Servant.Pagination.Internal where

import           Data.Kind      (Constraint)
import           Data.Proxy     (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text      (Text)
import           Servant        (FromHttpApiData (..), ToHttpApiData (..))


-- | Helper to execute two `Either a b` successively
orElse :: Either a b -> Either a b -> Either a b
orElse a b =
  either (const b) (const a) a
{-# INLINE orElse #-}


-- | Representation of AcceptRanges as a list of comma-separated text fields from the type
-- of a Range only (value isn't needed here since only the 'field' matters)
class ToAcceptRanges r where
  toAcceptRanges :: Proxy r -> Text


-- | Combine two ranges in a new range, parsing is done left-first
data a :|: b = InL a | InR b
infixl 7 :|:

instance (ToHttpApiData a, ToHttpApiData b) => ToHttpApiData (a :|: b) where
  toUrlPiece (InL a) =
    toUrlPiece a

  toUrlPiece (InR b) =
    toUrlPiece b

instance (FromHttpApiData a, FromHttpApiData b) => FromHttpApiData (a :|: b) where
  parseUrlPiece txt =
             (liftRange <$> (parseUrlPiece txt :: Either Text a))
    `orElse` (liftRange <$> (parseUrlPiece txt :: Either Text b))

instance (ToAcceptRanges a, ToAcceptRanges b) => ToAcceptRanges (a :|: b) where
  toAcceptRanges _ =
    toAcceptRanges (Proxy :: Proxy a) <> "," <>  toAcceptRanges (Proxy :: Proxy b)


-- | Type family helper to define a constraint about ranges
type family InRanges r rs :: Constraint where
  InRanges r r           = ()
  InRanges r (rs :|: r)  = ()
  InRanges r (rs :|: r') = InRanges r rs


-- | Relation for lifting range into a combination of ranges
class range :<: ranges where
  liftRange :: range -> ranges

instance r :<: r where
  liftRange = id
  {-# INLINE liftRange #-}

instance r :<: (r :|: r2) where
  liftRange = InL
  {-# INLINE liftRange #-}

instance r :<: (r1 :|: r) where
  liftRange = InR
  {-# INLINE liftRange #-}

instance r :<: (r1 :|: r :|: r3) where
  liftRange = InL . InR
  {-# INLINE liftRange #-}

instance r :<: (r :|: r2 :|: r3) where
  liftRange = InL . InL
  {-# INLINE liftRange #-}

instance r :<: (r1 :|: r :|: r3 :|: r4) where
  liftRange = InL . InL . InR
  {-# INLINE liftRange #-}

instance r :<: (r :|: r2 :|: r3 :|: r4) where
  liftRange = InL . InL . InL
  {-# INLINE liftRange #-}

instance r :<: (r1 :|: r :|: r3 :|: r4 :|: r5) where
  liftRange = InL . InL . InL . InR
  {-# INLINE liftRange #-}

instance r :<: (r :|: r2 :|: r3 :|: r4 :|: r5) where
  liftRange = InL . InL . InL . InL
  {-# INLINE liftRange #-}

instance r :<: (r1 :|: r :|: r3 :|: r4 :|: r5 :|: r6) where
  liftRange = InL . InL . InL . InL . InR
  {-# INLINE liftRange #-}

instance r :<: (r :|: r2 :|: r3 :|: r4 :|: r5 :|: r6) where
  liftRange = InL . InL . InL . InL . InL
  {-# INLINE liftRange #-}

instance r :<: (r1 :|: r :|: r3 :|: r4 :|: r5 :|: r6 :|: r7) where
  liftRange = InL . InL . InL . InL . InL . InR
  {-# INLINE liftRange #-}

instance r :<: (r :|: r2 :|: r3 :|: r4 :|: r5 :|: r6 :|: r7) where
  liftRange = InL . InL . InL . InL . InL . InL
  {-# INLINE liftRange #-}

instance r :<: (r1 :|: r :|: r3 :|: r4 :|: r5 :|: r6 :|: r7 :|: r8) where
  liftRange = InL . InL . InL . InL . InL . InL . InR
  {-# INLINE liftRange #-}

instance r :<: (r :|: r2 :|: r3 :|: r4 :|: r5 :|: r6 :|: r7 :|: r8) where
  liftRange = InL . InL . InL . InL . InL . InL . InL
  {-# INLINE liftRange #-}

instance r :<: (r1 :|: r :|: r3 :|: r4 :|: r5 :|: r6 :|: r7 :|: r8 :|: r9) where
  liftRange = InL . InL . InL . InL . InL . InL . InL . InR
  {-# INLINE liftRange #-}

instance r :<: (r :|: r2 :|: r3 :|: r4 :|: r5 :|: r6 :|: r7 :|: r8 :|: r9) where
  liftRange = InL . InL . InL . InL . InL . InL . InL . InL
  {-# INLINE liftRange #-}

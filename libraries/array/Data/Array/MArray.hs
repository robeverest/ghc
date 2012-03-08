-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.MArray
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.Base)
--
-- An overloaded interface to mutable arrays.  For array types which can be
-- used with this interface, see "Data.Array.IO", "Data.Array.ST",
-- and "Data.Array.Storable".
--
-----------------------------------------------------------------------------

module Data.Array.MArray (
    -- * Class of mutable array types
    MArray,       -- :: (* -> * -> *) -> * -> (* -> *) -> class

    -- * The @Ix@ class and operations
    module Data.Ix,

    -- * Constructing mutable arrays
    newArray,     -- :: (MArray a e m, Ix i) => (i,i) -> e -> m (a i e)
    newArray_,    -- :: (MArray a e m, Ix i) => (i,i) -> m (a i e)
    newListArray, -- :: (MArray a e m, Ix i) => (i,i) -> [e] -> m (a i e)

    -- * Reading and writing mutable arrays
    readArray,    -- :: (MArray a e m, Ix i) => a i e -> i -> m e
    writeArray,   -- :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()

    -- * Derived arrays
    mapArray,     -- :: (MArray a e' m, MArray a e m, Ix i) => (e' -> e) -> a i e' -> m (a i e)
    mapIndices,   -- :: (MArray a e m, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> m (a i e)

    -- * Deconstructing mutable arrays
    getBounds,    -- :: (MArray a e m, Ix i) => a i e -> m (i,i)
    getElems,     -- :: (MArray a e m, Ix i) => a i e -> m [e]
    getAssocs,    -- :: (MArray a e m, Ix i) => a i e -> m [(i, e)]

    -- * Conversions between mutable and immutable arrays
    freeze,       -- :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
    unsafeFreeze, -- :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
    thaw,         -- :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
    unsafeThaw,   -- :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
  ) where

import Data.Ix
import Data.Array.Base hiding ( unsafeFreeze, unsafeThaw )
import qualified Data.Array.Base as U ( unsafeFreeze, unsafeThaw )
#ifdef __HADDOCK__
import Data.Array.IArray
#endif

{-# DEPRECATED unsafeFreeze, unsafeThaw
              "Please import from Data.Array.Unsafe instead; This will be removed in the next release"
 #-}

{- |
   Converts an mutable array into an immutable array.  The
   implementation may either simply cast the array from
   one type to the other without copying the array, or it
   may take a full copy of the array.

   Note that because the array is possibly not copied, any subsequent
   modifications made to the mutable version of the array may be
   shared with the immutable version.  It is safe to use, therefore, if
   the mutable version is never modified after the freeze operation.

   The non-copying implementation is supported between certain pairs
   of array types only; one constraint is that the array types must
   have identical representations.  In GHC, The following pairs of
   array types have a non-copying O(1) implementation of
   'unsafeFreeze'.  Because the optimised versions are enabled by
   specialisations, you will need to compile with optimisation (-O) to
   get them.

     * 'Data.Array.IO.IOUArray' -> 'Data.Array.Unboxed.UArray'

     * 'Data.Array.ST.STUArray' -> 'Data.Array.Unboxed.UArray'

     * 'Data.Array.IO.IOArray' -> 'Data.Array.Array'

     * 'Data.Array.ST.STArray' -> 'Data.Array.Array'
-}
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
unsafeFreeze = U.unsafeFreeze

{- |
   Converts an immutable array into a mutable array.  The
   implementation may either simply cast the array from
   one type to the other without copying the array, or it
   may take a full copy of the array.

   Note that because the array is possibly not copied, any subsequent
   modifications made to the mutable version of the array may be
   shared with the immutable version.  It is only safe to use,
   therefore, if the immutable array is never referenced again in this
   thread, and there is no possibility that it can be also referenced
   in another thread.  If you use an unsafeThaw/write/unsafeFreeze
   sequence in a multi-threaded setting, then you must ensure that
   this sequence is atomic with respect to other threads, or a garbage
   collector crash may result (because the write may be writing to a
   frozen array).

   The non-copying implementation is supported between certain pairs
   of array types only; one constraint is that the array types must
   have identical representations.  In GHC, The following pairs of
   array types have a non-copying O(1) implementation of
   'unsafeThaw'.  Because the optimised versions are enabled by
   specialisations, you will need to compile with optimisation (-O) to
   get them.

     * 'Data.Array.Unboxed.UArray' -> 'Data.Array.IO.IOUArray'

     * 'Data.Array.Unboxed.UArray' -> 'Data.Array.ST.STUArray'

     * 'Data.Array.Array'  -> 'Data.Array.IO.IOArray'

     * 'Data.Array.Array'  -> 'Data.Array.ST.STArray'
-}
{-# INLINE unsafeThaw #-}
unsafeThaw :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
unsafeThaw = U.unsafeThaw

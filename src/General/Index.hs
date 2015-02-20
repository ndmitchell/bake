
module General.Index(
    Index, newIndex, addIndex, askIndex
    ) where

import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe


data Index filt val = Index
    (filt -> val -> Bool)
    (Int, [val])
    (IORef (Map.Map filt (Int, [val])))
-- for each list of filters, store the number you have processed, and the number that matched
-- [] is everything
-- throw away filters which are more than half empty on an add
--     (is that necessary? by definition, they are relatively small)

newIndex :: (filt -> val -> Bool) -> Index filt val
newIndex filt = Index filt (0, []) $ unsafePerformIO $ newIORef Map.empty

addIndex :: val -> Index filt val -> Index filt val
addIndex x (Index filt (n,xs) ref) = unsafePerformIO $ do
    ref <- newIORef =<< readIORef ref
    return $ Index filt (n+1, x:xs) ref

askIndex :: Ord filt => filt -> Index filt val -> [val]
askIndex p (Index filt (n,xs) ref) = unsafePerformIO $ atomicModifyIORef ref $ \mp ->
    let (n',xs') = Map.findWithDefault (0,[]) p mp
        xs2 = filter (filt p) (take (n-n') xs) ++ xs'
    in (Map.insert p (n,xs2) mp, xs2)

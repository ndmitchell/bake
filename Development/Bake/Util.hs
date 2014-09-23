
module Development.Bake.Util(
    sleep, newCookie, timed, withTempFile
    ) where


sleep :: Double -> IO ()
sleep = undefined


newCookie :: IO String
newCookie = undefined


timed :: IO a -> IO (Double, a)
timed = undefined


withTempFile :: String -> (FilePath -> IO ()) -> IO ()
withTempFile = undefined

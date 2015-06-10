{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Applicative ((<$>))
import           Control.Exception (try, SomeException)
import           Control.Lens (over, _Left, (&))
import           Control.Monad (void, when)
import           Crypto.Cipher.AES.Util (cbcEncrypt', cbcDecrypt')
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS (ByteString, readFile, writeFile)
import           Data.ByteString.UTF8 (toString)
import           Data.List (isInfixOf)
import           Data.String (fromString)
import           System.Environment (getArgs)

data Entry = E { name :: String, pass :: String, kv :: [(String, String)] } deriving (Eq, Ord, Show, Read)

-- TODO writer logs, like failed to open file or failed to decrypt
load :: FilePath -> ByteString -> IO [Entry]
load fn pass = do
    eContents <- over _Left (show :: SomeException -> String) <$> try (BS.readFile fn)
    return $ either (const []) (read . toString) (eContents >>= cbcDecrypt' pass)

-- TODO writer logs, like failed to open file or failed to decrypt
save :: FilePath -> ByteString -> [Entry] -> IO (Either String ())
save fn pass es = cbcEncrypt' pass (fromString . show $ es) >>=
                      either (return . Left) (fmap Right . BS.writeFile fn)

usage :: IO ()
usage = putStrLn $ "usage: 2password file passphrase get service-name\n" ++
                   "       2password file passphrase add service-name password"

main :: IO ()
main = do
    args <- getArgs
    if length args < 3 then
        usage
    else do
        (fn:pass:rest) <- getArgs
        run (save fn (fromString pass)) rest =<< load fn (fromString pass)

run :: ([Entry] -> IO (Either String ())) -> [String] -> [Entry] -> IO ()
run _    ("get":n:[])   es = print $ filter ((n `isInfixOf`) . name) es
run save ("add":n:p:[]) es = print =<< save (E { name = n, pass = p, kv = [] }:es)
run _    _              _  = usage

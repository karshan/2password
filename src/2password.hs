{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Applicative ((<$>))
import           Control.Exception (bracket_, try, SomeException)
import           Control.Lens (over, _Left, (&))
import           Control.Monad (unless, void, when)
import           Crypto.Cipher.AES.Util (cbcEncrypt', cbcDecrypt')
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS (ByteString, readFile, writeFile)
import           Data.ByteString.UTF8 (toString)
import           Data.List (isInfixOf)
import           Data.String (fromString)
import           System.Environment (getArgs)
import           System.IO
import           Data.Maybe
import           System.Hclip
import           Data.Bool (bool)
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import           Data.Monoid ((<>))
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Affine as Draw
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL

assert :: Monad m => String -> Bool -> m ()
assert msg p = unless p (fail msg)

getVideoModeSize :: IO (Int, Int)
getVideoModeSize = do
  monitor <- maybe (fail "GLFW: Can't get primary monitor") return =<< GLFW.getPrimaryMonitor
  videoMode <- maybe (fail "GLFW: Can't get video mode of monitor") return =<< GLFW.getVideoMode monitor
  return (GLFW.videoModeWidth videoMode, GLFW.videoModeHeight videoMode)

withGLFW :: IO a -> IO a
withGLFW = bracket_ (GLFW.init >>= assert "initialize failed") GLFW.terminate

createWindow :: String -> Maybe GLFW.Monitor -> (Int, Int) -> IO GLFW.Window
createWindow title mMonitor (w, h) = do
    mWin <- GLFW.createWindow w h title mMonitor Nothing
    case mWin of
        Nothing -> fail "Open window failed"
        Just win -> do
            GLFW.makeContextCurrent $ Just win
            return win

charcb :: IORef String -> GLFW.Window -> Char -> IO ()
charcb ref win k = modifyIORef ref (++ [k])

keycb :: IORef String -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keycb ref win GLFW.Key'Enter _ _ _ = (readIORef ref >>= maincb) >> GLFW.setWindowShouldClose win True
keycb _ _ _ _ _ _ = return ()

glmain :: IO ()
glmain = do
  passioref <- newIORef ""
  withGLFW $ do
    win <- createWindow "test" Nothing (400, 400) -- =<< getVideoModeSize
    GLFW.setCharCallback win (Just $ charcb passioref)
    GLFW.setKeyCallback win (Just $ keycb passioref)
    Draw.withFont "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" (mainLoop win)

mainLoop :: GLFW.Window -> Draw.Font -> IO ()
mainLoop win font =
  GLFW.windowShouldClose win >>= flip bool (return ()) (do
    (w, h) <- GLFW.getFramebufferSize win
    GL.viewport $= (GL.Position 0 0,
                    GL.Size (fromIntegral w) (fromIntegral h))
    render win font
    GLFW.swapBuffers win
    GLFW.pollEvents
    mainLoop win font
  )

render :: GLFW.Window -> Draw.Font -> IO ()
render win font = do
    Draw.clearRender (Draw.translate (-1,0) <> Draw.scale 0.125 0.125 %% Draw.text font "enter2password")

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
usage = putStr $ "usage: 2password file get service-name\n" ++
                 "       2password file add service-name\n" ++
                 "       2password file list\n" ++
                 "       2password file show\n"

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then
        usage
    else do
        glmain

maincb pass = do
    (filename:rest) <- getArgs
    run (save filename (fromString pass)) rest =<< load filename (fromString pass)


run :: ([Entry] -> IO (Either String ())) -> [String] -> [Entry] -> IO ()
run _    ["get", n] es = maybe (putStrLn "I got nothing") (setClipboard . pass) $ listToMaybe $ filter ((n `isInfixOf`) . name) es
run save ["add", n] es = getPassword >>= (\p -> save (E n p []:es)) >>= print
run _    ["list"]   es = mapM_ print es
run _    ["show"]   es = print es
run _    _          _  = usage

module Microcontest (
  MicrocontestM,
  runMicrocontestM,
  startContest,
  showSolution,
  submitSolution,
  getInt,
  getDouble,
  getString,
  setInt,
  setDouble,
  setString
) where

import System.IO
import Control.Exception
import Control.Monad.State
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Network.HTTP
import Network.HTTP.Cookie
import Network.URI (parseURI)
import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA


type UserName = String
type VarName  = String

data ContestState = ContestState { contestId      :: Int
                                 , contestCookies :: [Cookie]
                                 , contestRVars   :: Map.Map VarName String
                                 , contestWVars   :: Map.Map VarName String
                                 }
                    
type MicrocontestM a = StateT ContestState IO a

emptyState :: ContestState
emptyState = ContestState 0 [] Map.empty Map.empty

runMicrocontestM :: MicrocontestM a -> IO a
runMicrocontestM m = evalStateT m emptyState

startContest :: UserName -> Int -> MicrocontestM ()
startContest pseudo cid = do
  modify $ \st -> st { contestId = cid }
  let url = "http://www.microcontest.com/contests/" ++ show cid ++ "/contest.php"
  passwrd <- liftIO getPassword
  let reqBody = "username=" ++ pseudo ++ "&" ++
                "password=" ++ showDigest (sha1 (pack passwrd)) ++ "&" ++
                "ID=" ++ show cid ++ "&" ++
                "contestlogin=1" ++ "&" ++
                "version=2"
  let req = Request { rqURI = fromJust $ parseURI url
                    , rqMethod = POST :: RequestMethod
                    , rqHeaders = [ mkHeader HdrContentType "application/x-www-form-urlencoded"
                                  , mkHeader HdrContentLength $ show $ length reqBody
                                  ]
                    , rqBody = reqBody
                    }
  rsp      <- liftIO $ simpleHTTP req
  respBody <- liftIO $ getResponseBody rsp
  liftIO $ putStrLn $ "problem    : " ++ respBody
  let cookies = snd $ case rsp of
                        Left _  -> error "http error"
                        Right r -> processCookieHeaders "PHPSESSID" $ rspHeaders r
  modify $ \st -> st { contestCookies = cookies }
  modify $ \st -> st { contestRVars = readVariables respBody }
  return ()

showSolution :: MicrocontestM ()
showSolution = do
  wVars <- liftM contestWVars get
  let reqBody = showVariables wVars
  liftIO $ putStrLn $ "proposition: " ++ reqBody

submitSolution :: MicrocontestM ()
submitSolution = do
  ContestState cid cookies _ wVars <- get
  let url = "http://www.microcontest.com/contests/" ++ show cid ++ "/validation.php"
  let reqBody = showVariables wVars
  let req = Request { rqURI = fromJust $ parseURI url
                    , rqMethod = POST :: RequestMethod
                    , rqHeaders = [ mkHeader HdrContentType "application/x-www-form-urlencoded"
                                  , mkHeader HdrContentLength $ show $ length reqBody
                                  , cookiesToHeader cookies
                                  ]
                    , rqBody = reqBody
                    }
  respBody <- liftIO $ simpleHTTP req >>= getResponseBody
  liftIO $ putStrLn $ "submission : " ++ reqBody
  liftIO $ putStrLn respBody

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getInt :: VarName -> MicrocontestM Int
getInt name = do
  st <- get
  return . read $ contestRVars st Map.! name

getDouble :: VarName -> MicrocontestM Double
getDouble name = do
  st <- get
  return . read $ contestRVars st Map.! name

getString :: VarName -> MicrocontestM String
getString name = do
  st <- get
  return $ contestRVars st Map.! name

setInt :: VarName -> Int -> MicrocontestM ()
setInt name val =
  modify $ \st -> st { contestWVars = Map.insert name (show val) $ contestWVars st }

setDouble :: VarName -> Double -> MicrocontestM ()
setDouble name val =
  modify $ \st -> st { contestWVars = Map.insert name (show val) $ contestWVars st }

setString :: VarName -> String -> MicrocontestM ()
setString name val =
  modify $ \st -> st { contestWVars = Map.insert name val $ contestWVars st }

readVariables :: String -> Map.Map VarName String
readVariables str = decodeList . tail $ splitOn "<br/>" str
  where decodeList :: [String] -> Map.Map String String
        decodeList [""] = Map.empty
        decodeList (x:y:z:xs) =
          let name = tail $ init x in
          case y of
            'L':'o':'n':'g':'u':'e':'u':'r':_ ->
              let val = splitOn "=" z !! 1 in
              Map.insert name val $ decodeList xs
            'V':'a':'l':'e':'u':'r':_         ->
              let val = splitOn "=" y !! 1 in
              Map.insert name val $ decodeList (z:xs)
            _ -> error "parse error #1"
        decodeList _ = error "parse error #2"

showVariables :: Map.Map VarName String -> String
showVariables vars = intercalate "&" $ map (\(n,v) -> n ++ "=" ++ v) $ Map.assocs vars

import Microcontest

main :: IO ()
main = runMicrocontestM contest

contest :: MicrocontestM ()
contest = do
  startContest "ad_0__" 1
  a <- getInt "a"
  b <- getInt "b"
  setInt "s" $ a + b
  submitSolution

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module System.Extras where

import Control.Applicative (pure)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either (EitherT, eitherT, left, right)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- | A monad appropriate for system calls. Runs in IO, and in the case of error,
-- returns a CommandResult in Left.
type System = EitherT CommandResult IO

-- | The results from a command.
type CommandResult = ( ExitCode  -- ^ Exit code.
                     , String    -- ^ stdout.
                     , String    -- ^ stderr.
                     )

-- | Runs a command with args and input.
systemCall :: String    -- ^ The command to run.
           -> [String]  -- ^ The arguments.
           -> String    -- ^ The input.
           -> System CommandResult
systemCall command args input =
    liftIO (readProcessWithExitCode command args input) >>=
        \case
            result@(ExitSuccess,   _, _) -> right result
            result@(ExitFailure _, _, _) -> left result

-- | Like 'systemCall', but with no input.
systemCall' :: String    -- ^ The command to run.
            -> [String]  -- ^ The arguments.
            -> System CommandResult
systemCall' command args = systemCall command args ""

-- | Like 'systemCall', but if the command returns ExitFailure, return a default value in IO. Otherwise, return the
-- output in IO.
systemCallWithDefault :: String    -- ^ Default value
                      -> String    -- ^ The command to run.
                      -> [String]  -- ^ The arguments.
                      -> String    -- ^ The input.
                      -> IO String
systemCallWithDefault def command args input =
    eitherT onFailure onSuccess $ systemCall command args input
  where
    onFailure :: CommandResult -> IO String
    onFailure _ = pure def

    onSuccess :: CommandResult -> IO String
    onSuccess (_, out, _) = pure out

-- Like 'systemCall', but a success is a failure and vice versa.
systemCallReverse :: String -> [String] -> String -> System CommandResult
systemCallReverse command args input =
    liftIO (readProcessWithExitCode command args input) >>=
        \case
            result@(ExitSuccess,   _, _) -> left result
            result@(ExitFailure _, _, _) -> right result

-- Like 'systemCallReverse', but with no input.
systemCallReverse' :: String -> [String] -> System CommandResult
systemCallReverse' command args = systemCallReverse command args []

-- Like 'systemCall', but if the command returns ExitFailure, print stderr and exit. Otherwise, return the output in IO.
fatalCall :: String -> [String] -> String -> IO String
fatalCall command args input =
    eitherT onFailure onSuccess $ systemCall command args input
  where
    onFailure :: CommandResult -> IO String
    onFailure (exit_code, _, err) =
        hPutStrLn stderr (unwords (command:args) ++ " failed: " ++ err) >>
        exitWith exit_code

    onSuccess :: CommandResult -> IO String
    onSuccess (_, out, _) = pure out

-- Like 'fatalCall', but with no input.
fatalCall' :: String -> [String] -> IO String
fatalCall' command args = fatalCall command args []

touchFile :: FilePath -> IO ()
touchFile file_path = writeFile file_path ""

module Main (main) where

import Korektor.Cli (Command (..), parseCommand, runCommand)
import Korektor.Lsp (runLsp)
import System.Exit (exitWith)

main :: IO ()
main =
  parseCommand >>= \case
    CmdLsp -> runLsp >>= exitWith
    cmd -> runCommand cmd >>= exitWith

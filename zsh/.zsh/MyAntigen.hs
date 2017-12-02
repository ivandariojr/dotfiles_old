{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module MyAntigen where

import Antigen (
                -- Rudimentary imports
                AntigenConfig (..)
              , bundle
              , defaultConfig
              , antigen
                -- If you want to source a bit trickier plugins
              , ZshPlugin (..)
              , antigenSourcingStrategy
              , filePathsSourcingStrategy
              )
import Shelly (shelly)

bundles =
  [ bundle "Tarrasch/zsh-functional"
  , bundle "zsh-users/zsh-syntax-highlighting"
  , (bundle "zsh-users/zsh-history-substring-search"){ sourcingStrategy = antigenSourcingStrategy }
  , bundle "srijanshetty/zsh-pip-completion"
  , (bundle "robbyrussell/oh-my-zsh")
    { sourcingStrategy = filePathsSourcingStrategy
                         [ "lib/completion.zsh"
                         , "lib/functions.zsh"
                         , "lib/key-bindings.zsh"
                         , "lib/misc.zsh"
                         , "lib/spectrum.zsh"
                         , "lib/git.zsh"
                         , "lib/theme-and-appearance.zsh"
                         , "themes/agnoster.zsh-theme"
                         , "plugins/rsync"
                         , "plugins/git"
--                         , "plugins/debian"
                         ] }
  ]
config = defaultConfig { plugins = bundles }

main :: IO ()
main = antigen config

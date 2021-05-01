module Astrid.Tech.CLI (commandParser) where

import Options.Applicative

data Command = Build | Development

commandParser :: Parser Command
commandParser =
  hsubparser
    ( (command "create" (info buildOptions (progDesc "Fully rebuild the site")))
        <> (command "dev" (info devOptions (progDesc "Start development server")))
    )

buildOptions :: Parser Command
buildOptions = pure Build

devOptions :: Parser Command
devOptions = pure Development

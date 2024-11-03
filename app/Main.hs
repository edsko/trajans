module Main (main) where

import Options.Applicative

import Diagrams.Backend.CmdLine
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

import Trajans.RenderOptions
import Trajans.Text
import Trajans.Util.Diagrams

main :: IO ()
main = mainWith (growBoundingBox (r2 (1, 1)) . execRenderCommand)

execRenderCommand :: RenderCommand -> Diagram B
execRenderCommand (RenderText opts t) = renderText opts t

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data RenderCommand = RenderText RenderOptions String
  deriving (Show)

instance Parseable RenderCommand where
  parser = RenderText
      <$> parseRenderOptions
      <*> ( strOption $ mconcat [
                long "text"
              , help "Text to render"
              ])

{-------------------------------------------------------------------------------
  Rendering text
-------------------------------------------------------------------------------}

renderText :: RenderOptions -> String -> Diagram B
renderText opts input =
    case mapM constructLine (lines input) of
      Left (Error err) ->
        error $ "Error: " ++ err
      Right parsed ->
          mconcat
        . map (uncurry translateY)
        . zip [0, 0 - interLineSpacing ..]
        . map (renderLine opts)
        $ parsed
  where
    interLineSpacing :: Double
    interLineSpacing = 13

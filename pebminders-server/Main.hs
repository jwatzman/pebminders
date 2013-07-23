module Main(main) where

import Control.Monad (msum)
import qualified Happstack.Lite as S

import qualified Page.Home

dispatch :: S.ServerPart S.Response
dispatch =
	msum [
		S.nullDir >> Page.Home.render
	]

main :: IO ()
main = do
	let config = Just $ S.defaultServerConfig {S.port = 4545}
	S.serve config dispatch

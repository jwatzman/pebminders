{-# LANGUAGE OverloadedStrings #-}

module Page.Home(render) where

import qualified Happstack.Lite as S
import qualified Text.Blaze.Html5 as H

render :: S.ServerPart S.Response
render = S.ok $ S.toResponse $
	H.docTypeHtml $ do
		H.head $ do
			H.title "PebMinders"
		H.body $ do
			H.h1 "PebMinders"

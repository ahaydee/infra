{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Auxiliar where

import Import
import Text.Cassius
import Text.Julius
import Text.Lucius
--import Network.HTTP.Types.Status
--import Database.Persist.Postgresql

formWidget :: Maybe (Route App) -> Route App -> Text -> Text -> Widget -> Maybe Html -> Widget
formWidget voltar rota name btn widget msg = $(whamletFile "templates/form.hamlet")

menu :: [(Route App, Text)] -> Text -> Widget
menu [] cl = [whamlet|
                <h1>
                    ERRO
            |]
menu rs cl = [whamlet|
            <ul class=#{cl}>
                $forall (rota,nome) <- rs
                    <li class="nav-item active">
                        <a class="nav-link" href=@{rota}>
                            #{nome}
        |]
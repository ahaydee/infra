{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Cassius
import Text.Julius
import Text.Lucius
--import Network.HTTP.Types.Status
--import Database.Persist.Postgresql


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

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        toWidgetHead $(juliusFile "templates/home.julius")
        addScriptRemote "https://kit.fontawesome.com/2ab210d476.js"
        addStylesheetRemote "https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@300;400;600;700"
        addStylesheet (StaticR css_bootstrapgrid_css)
        addStylesheet (StaticR css_bootstrapgrid_min_css)
        addStylesheet (StaticR css_bootstrapgrid_rtl_css)
        addStylesheet (StaticR css_bootstrapgrid_rtl_min_css)
        addStylesheet (StaticR css_bootstrapreboot_css)
        addStylesheet (StaticR css_bootstrapreboot_min_css)
        addStylesheet (StaticR css_bootstrapreboot_rtl_css)
        addStylesheet (StaticR css_bootstrapreboot_rtl_min_css)
        addStylesheet (StaticR css_bootstraputilities_css)
        addStylesheet (StaticR css_bootstraputilities_min_css)
        addStylesheet (StaticR css_bootstraputilities_rtl_css)
        addStylesheet (StaticR css_bootstraputilities_rtl_min_css)
        addStylesheet (StaticR css_bootstrap_css)
        addStylesheet (StaticR css_bootstrap_min_css)
        addStylesheet (StaticR css_bootstrap_rtl_css)
        addStylesheet (StaticR css_bootstrap_rtl_min_css)
        addStylesheet (StaticR js_bootstrap_bundle_js)
        addStylesheet (StaticR js_bootstrap_bundle_min_js)
        addStylesheet (StaticR js_bootstrap_esm_js)
        addStylesheet (StaticR js_bootstrap_esm_min_js)
        addStylesheet (StaticR js_bootstrap_js)
        addStylesheet (StaticR js_bootstrap_min_js)
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menu.hamlet")
        $(whamletFile "templates/home.hamlet")
        $(whamletFile "templates/footer.hamlet")
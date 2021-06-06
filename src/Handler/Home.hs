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
import Handler.Auxiliar
--import Network.HTTP.Types.Status
--import Database.Persist.Postgresql

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,)
    <$> areq emailField (FieldSettings "E-mail: " Nothing (Just "email") Nothing [("class", "form-control")]) Nothing
    <*> areq passwordField (FieldSettings "Senha: " Nothing (Just "senha") Nothing [("class", "form-control")]) Nothing

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        logged <- lookupSession "_ID"
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

getEntrarR :: Handler Html
getEntrarR = do
    (widget, _) <- generateFormPost formLogin
    msg <- getMessage -- Handler (Maybe Text)
    defaultLayout $ do
        logged <- lookupSession "_ID"
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
        ((formWidget Nothing (EntrarR) "Login" "Entrar" widget msg ))
        $(whamletFile "templates/footer.hamlet")

postEntrarR :: Handler Html
postEntrarR = do 
    ((result,_),_) <- runFormPost formLogin
    case result of 
        FormSuccess ("admin@root.com","root123") -> do 
            setSession "_ID" "admin"
            redirect RequisicoesR
        FormSuccess (email,senha) -> do 
           usuario <- runDB $ getBy (UniqueEmail email)
           case usuario of 
                Nothing -> do 
                    setMessage [shamlet|
                        <p style="color: #f44336;font-weight: bold;">
                            Usuário e/ou Senha não conferem
                    |]
                    redirect EntrarR
                Just (Entity _ usu) -> do 
                    if (usuarioSenha usu == senha) then do
                        setSession "_ID" (usuarioEmail usu)
                        redirect CatalogoR
                    else do 
                        setMessage [shamlet|
                            <p style="color: #f44336;font-weight: bold;">
                                Usuário e/ou Senha não conferem
                        |]
                        redirect EntrarR 
        _ -> do
            setMessage [shamlet|
                <p style="color: #f44336;font-weight: bold;">
                    Usuário e/ou Senha não conferem
            |]
            redirect EntrarR 

postSairR :: Handler Html 
postSairR = do 
    deleteSession "_ID"
    redirect EntrarR
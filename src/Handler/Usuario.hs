{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import Import
import Text.Cassius
import Text.Julius
import Text.Lucius

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

--categorys = do
--       entidades <- runDB $ selectList [] [Asc CategoriaNome] 
--       optionsPairs $ fmap (\ent -> (categoriaNome $ entityVal ent, entityKey ent)) entidades

{-formUsuario :: Form Usuario
formUsuario = renderDivs $ Usuario 
    <$> areq textField  (FieldSettings "Nome: " Nothing (Just "nome") Nothing [("class", "form-control")]) Nothing
    <*> areq textField  (FieldSettings "Email: " Nothing (Just "email") Nothing [("class", "form-control")]) Nothing
    <*> areq passwordConfirmField   (FieldSettings "Senha: " Nothing (Just "senha") Nothing [("class", "form-control")]) Nothing
    <*> areq (selectFieldList tipo) "Tipo" Nothing
    <*> areq (selectFieldList departamento) "Departamento" Nothing
      where
        tipo = do
           entidades <- runDB $ selectList [] [Asc TipoUsuarioNome] 
           map (\(Entity cid cat) -> (tipoUsuarioNome cat, cid)) entidades
        departamento = do
           entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
           map (\(Entity cid cat) -> (departamentoNome cat, cid)) entidades-}

getUsuarioR :: Handler Html
getUsuarioR = do
    {-(widget, _) <- generateFormPost formUsuario-}
                                        {-^{widget}-}
    msg <- getMessage -- Handler (Maybe Text)
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
        [whamlet| 
            <main>
                <div class="container content" style="margin-bottom: 55px">
                    <div class="row" style="padding-left: 15px">
                        <div class="col-lg-12" style="margin-top: 30px">
                            <h3>
                                Adicionar Usuário

                        $maybe mensa <- msg
                            <div class="col-lg-12">
                                <p>
                                    ^{mensa}

                        <div class="col-lg-12">
                            <form action=@{UsuarioR} method=get>
                                <div class="row">
                                    <div class="col-lg-4">

                                <div class="col-lg-12">
                                    <div class="row">
                                        <div class="col-lg-2">
                                            <br>
                                            <input type="submit" value="Cadastrar" class="form-control" >
        |]
        $(whamletFile "templates/footer.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
    {-((result, _), _) <- runFormPost formUsuario
    case result of 
        FormSuccess usuario -> do
            runDB $ insert usuario
            setMessage [shamlet|
                    <div>
                        Usuário inserido com sucesso
            |]
            redirect $ UsuarioR
        _ -> redirect HomeR-}
    redirect $ UsuarioR

postApagarUsuarioR :: UsuarioId -> Handler Html
postApagarUsuarioR id = do
    runDB $ delete id
    redirect $ UsuariosR

getListarUsuarioR ::UsuarioId -> Handler Html
getListarUsuarioR uid = do
    usuario <- runDB $ get404 uid
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
        $(whamletFile "templates/usuario.hamlet")
        $(whamletFile "templates/footer.hamlet")


getUsuariosR :: Handler Html
getUsuariosR = do
    usuarios <- runDB $ selectList [] [Asc UsuarioNome]
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
        $(whamletFile "templates/usuarios.hamlet")
        $(whamletFile "templates/footer.hamlet")

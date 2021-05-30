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
import Handler.Auxiliar
import Database.Persist.Postgresql

formUsuario :: Maybe Usuario -> Form Usuario
formUsuario mc = renderDivs $ Usuario 
    <$> areq textField  (FieldSettings "Nome: " Nothing (Just "nome") Nothing [("class", "form-control")]) (fmap usuarioNome mc)
    <*> areq textField  (FieldSettings "Email: " Nothing (Just "email") Nothing [("class", "form-control")]) (fmap usuarioEmail mc)
    <*> areq passwordField   (FieldSettings "Senha: " Nothing (Just "senha") Nothing [("class", "form-control")]) (fmap usuarioSenha mc)
    <*> areq (selectField departamento) (FieldSettings "Departamento: " Nothing (Just "departamento") Nothing [("class", "form-select")]) (fmap usuarioDid mc)
      where
        departamento = do
           entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
           optionsPairs $ map (\(Entity cid cat) -> (departamentoNome cat, cid)) entidades

getUsuarioR :: Handler Html
getUsuarioR = do
    (widget, _) <- generateFormPost (formUsuario Nothing)
    msg <- getMessage -- Handler (Maybe Text)
    logged <- lookupSession "_ID"
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
        ((formWidget (Just UsuariosR) (UsuarioR) "Adicionar Usuário" "Cadastrar" widget msg))
        $(whamletFile "templates/footer.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result, _), _) <- runFormPost (formUsuario Nothing)
    case result of 
        FormSuccess usuario -> do
            runDB $ insert usuario
            setMessage [shamlet|
                    <div>
                        Usuário inserido com sucesso
            |]
            redirect $ UsuarioR
        _ -> redirect HomeR

postApagarUsuarioR :: UsuarioId -> Handler Html
postApagarUsuarioR id = do
    runDB $ delete id
    redirect $ UsuariosR

getListarUsuarioR ::UsuarioId -> Handler Html
getListarUsuarioR uid = do
    usu <- runDB $ get404 uid
    let sql = "SELECT ??,?? FROM usuario \
          \ INNER JOIN departamento ON usuario.did = departamento.id \
          \ WHERE usuario.id = ?"
    usuario <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity Departamento)]
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
        $(whamletFile "templates/usuario.hamlet")
        $(whamletFile "templates/footer.hamlet")


getUsuariosR :: Handler Html
getUsuariosR = do
    let sql = "SELECT ??,?? FROM usuario \
          \ INNER JOIN departamento ON usuario.did = departamento.id"
    usuarios <- runDB $ rawSql sql [] :: Handler [(Entity Usuario,Entity Departamento)]
    logged <- lookupSession "_ID"
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


getEditarUsuarioR :: UsuarioId -> Handler Html
getEditarUsuarioR uid = do
    usuario <- runDB $ get404 uid
    (widget, _) <- generateFormPost (formUsuario (Just usuario))
    msg <- getMessage -- Handler (Maybe Text)
    logged <- lookupSession "_ID"
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
        ((formWidget (Just UsuariosR) (EditarUsuarioR uid) "Editar Usuario" "Salvar" widget msg))
        $(whamletFile "templates/footer.hamlet")


postEditarUsuarioR :: UsuarioId -> Handler Html
postEditarUsuarioR uid = do
    _ <- runDB $ get404 uid
    ((result, _), _) <- runFormPost (formUsuario Nothing)
    case result of
        FormSuccess novoUsuario -> do 
            runDB $ replace uid novoUsuario
            redirect UsuariosR
        _ -> redirect UsuariosR

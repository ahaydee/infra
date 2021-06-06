{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Servico where

import Import
import Text.Cassius
import Text.Julius
import Text.Lucius
import Handler.Auxiliar
import Database.Persist.Postgresql
--import Network.HTTP.Types.Status
--import Database.Persist.Postgresql

formServico :: CategoriaId -> Maybe Servico -> Form Servico
formServico cid mc = renderDivs $ Servico 
    <$> pure cid
    <*> areq textField  (FieldSettings "Nome: " Nothing (Just "nome") Nothing [("class", "form-control")]) (fmap servicoNome mc)
    <*> areq textareaField  (FieldSettings "Descrição: " Nothing (Just "descricao") Nothing [("class", "form-control")]) (fmap servicoDescricao mc)

getServicoR :: CategoriaId -> Handler Html
getServicoR cid = do
    (widget, _) <- generateFormPost (formServico cid Nothing)
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
        ((formWidget (Just (ServicosR cid)) (ServicoR cid) "Adicionar Serviço" "Cadastrar" widget msg ))
        $(whamletFile "templates/footer.hamlet")

postServicoR :: CategoriaId -> Handler Html
postServicoR cid = do
    ((result, _), _) <- runFormPost (formServico cid Nothing)
    case result of 
        FormSuccess servico -> do
            runDB $ insert servico
            setMessage [shamlet|
                    <p style="color: #673ab7;font-weight: bold;">
                        Serviço inserido com sucesso
            |]
            redirect $ ServicoR cid
        _ -> redirect $ HomeR

postApagarServicoR :: CategoriaId -> ServicoId -> Handler Html
postApagarServicoR cid id = do
    runDB $ delete id
    redirect $ ServicosR cid

getListarServicoR :: CategoriaId -> ServicoId -> Handler Html
getListarServicoR cid sid = do
    servico <- runDB $ get404 sid
    logged <- lookupSession "_ID"
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
        $(whamletFile "templates/servico.hamlet")
        $(whamletFile "templates/footer.hamlet")


getEditarServicoR :: CategoriaId ->  ServicoId -> Handler Html
getEditarServicoR cid sid = do
    servico <- runDB $ get404 sid
    (widget, _) <- generateFormPost (formServico cid (Just servico))
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
        ((formWidget (Just (ServicosR cid)) (EditarServicoR cid sid) "Editar Serviço" "Salvar" widget msg ))
        $(whamletFile "templates/footer.hamlet")


postEditarServicoR :: CategoriaId -> ServicoId -> Handler Html
postEditarServicoR cid sid = do
    _ <- runDB $ get404 sid
    ((result, _), _) <- runFormPost (formServico cid Nothing)
    case result of
        FormSuccess novoServico -> do 
            runDB $ replace sid novoServico
            redirect $ ServicosR cid
        _ -> redirect $ ServicosR cid
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

formServico :: Form Servico
formServico = renderDivs $ Servico 
    <$> areq textField  (FieldSettings "Nome: " Nothing (Just "nome") Nothing [("class", "form-control")]) Nothing
    <*> areq textareaField  (FieldSettings "Descrição: " Nothing (Just "descricao") Nothing [("class", "form-control")]) Nothing
    <*> areq (selectField cats) (FieldSettings "Categoria: " Nothing (Just "categoria") Nothing [("class", "form-select")]) Nothing
      where
        cats = do
           entidades <- runDB $ selectList [] [Asc CategoriaNome] 
           optionsPairs $ map (\(Entity cid cat) -> (categoriaNome cat, cid)) entidades

getServicoR :: CategoriaId -> Handler Html
getServicoR cid = do
    (widget, _) <- generateFormPost formServico
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
                                Adicionar Serviço

                        $maybe mensa <- msg
                            <div class="col-lg-12">
                                <p>
                                    ^{mensa}

                        <div class="col-lg-12">
                            <form action=@{ServicoR cid} method=post>
                                <div class="row">
                                    <div class="col-lg-4">
                                        ^{widget}

                                <div class="col-lg-12">
                                    <div class="row">
                                        <div class="col-lg-2">
                                            <br>
                                            <input type="submit" value="Cadastrar" class="form-control" >
        |]
        $(whamletFile "templates/footer.hamlet")

postServicoR :: CategoriaId -> Handler Html
postServicoR cid = do
    ((result, _), _) <- runFormPost formServico
    case result of 
        FormSuccess servico -> do
            runDB $ insert servico
            setMessage [shamlet|
                    <div>
                        Serviço inserido com sucesso
            |]
            redirect $ ServicoR cid
        _ -> redirect HomeR

postApagarServicoR :: CategoriaId -> ServicoId -> Handler Html
postApagarServicoR cid id = do
    runDB $ delete id
    redirect $ ServicosR cid

getListarServicoR :: CategoriaId -> ServicoId -> Handler Html
getListarServicoR cid sid = do
    servico <- runDB $ get404 sid
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
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Categoria where

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

formCategoria :: Form Categoria
formCategoria = renderDivs $ Categoria
    <$> areq textField  (FieldSettings "Nome: " Nothing (Just "name") Nothing [("class", "form-control")]) Nothing

getCategoriaR :: Handler Html
getCategoriaR = do
    (widget, _) <- generateFormPost formCategoria
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
                                Adicionar Categoria

                        $maybe mensa <- msg
                            <div class="col-lg-12">
                                <p>
                                    ^{mensa}

                        <div class="col-lg-12">
                            <form action=@{CategoriaR} method=post>
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

postCategoriaR :: Handler Html
postCategoriaR = do
    ((result, _), _) <- runFormPost formCategoria
    case result of 
        FormSuccess categoria -> do
            runDB $ insert categoria
            setMessage [shamlet|
                    <div>
                        Categoria inserida com sucesso
            |]
            redirect CategoriaR
        _ -> redirect HomeR

postApagarCategoriaR :: CategoriaId -> Handler Html
postApagarCategoriaR id = do
    runDB $ delete id
    redirect CatalogoR

getCatalogoR :: Handler Html
getCatalogoR = do
    categorias <- runDB $ selectList [] [Asc CategoriaNome]
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
        $(whamletFile "templates/catalogo.hamlet")
        $(whamletFile "templates/footer.hamlet")


getServicosR :: CategoriaId -> Handler Html
getServicosR cid = do
    categoria <- runDB $ get404 cid
    servicos <- runDB $ selectList [ServicoCid ==. cid] [Asc ServicoDescricao]
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
        $(whamletFile "templates/categoria.hamlet")
        $(whamletFile "templates/footer.hamlet")
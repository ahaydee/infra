{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Pedido where

import Import
import Text.Cassius
import Text.Julius
import Text.Lucius
import Data.Time.Clock
import Data.Time.Calendar
import Handler.Auxiliar
import Database.Persist.Postgresql

formPedido :: ServicoId -> UsuarioId -> SituacaoId -> Form Pedido
formPedido sid uid sit = renderDivs $ Pedido 
    <$> pure uid
    <*> pure sid
    <*> pure sit
    <*> lift (liftIO (map utctDay getCurrentTime))
    <*> areq textareaField  (FieldSettings "Descrição: " Nothing (Just "descricao") Nothing [("class", "form-control")]) Nothing

getSolicitarR :: CategoriaId -> ServicoId -> Handler Html
getSolicitarR cid sid = do
    servico <- runDB $ get404 sid
    logged <- lookupSession "_ID"
    case logged of 
        Just email -> do
            usuario <- runDB $ getBy (UniqueEmail email)
            case usuario of 
                Nothing -> do 
                    redirect EntrarR
                Just (Entity usuId usu) -> do 
                    (widget, _) <- generateFormPost (formPedido sid usuId (toSqlKey 1))
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
                        ((formWidget (Just (ListarServicoR cid sid)) (SolicitarR cid sid) "Solicitar Serviço" "Confirmar" widget msg ))
                        $(whamletFile "templates/footer.hamlet")
        _ -> redirect $ ServicosR cid

postSolicitarR :: CategoriaId -> ServicoId -> Handler Html
postSolicitarR cid sid = do
    _ <- runDB $ get404 sid
    logged <- lookupSession "_ID"
    case logged of 
        Just email -> do
            usuario <- runDB $ getBy (UniqueEmail email)
            case usuario of 
                Nothing -> do 
                    redirect EntrarR
                Just (Entity usuId usu) -> do 
                    ((result, _), _) <- runFormPost (formPedido sid usuId (toSqlKey 1))
                    case result of 
                        FormSuccess pedido -> do
                            runDB $ insert pedido
                            setMessage [shamlet|
                                <p style="color: #673ab7;font-weight: bold;">
                                    Serviço solicitado com sucesso
                            |]
                            redirect $ SolicitarR cid sid
                        _ -> redirect $ ServicosR cid
        _ -> redirect $ ServicosR cid

getRequisicaoR :: PedidoId -> Handler Html
getRequisicaoR pid = do
    _ <- runDB $ get404 pid
    msg <- getMessage -- Handler (Maybe Text)
    logged <- lookupSession "_ID"
    let sql = "SELECT ??,??,??,?? FROM pedido \
      \ INNER JOIN servico ON pedido.sid = servico.id \
      \ INNER JOIN situacao ON pedido.stid = situacao.id \
      \ INNER JOIN usuario ON pedido.uid = usuario.id \
      \ WHERE pedido.id = ?"
    pedidos <- do
        case logged of
            Just email -> do
                if (email /= "admin") then do
                    let sql2 = sql ++ " AND usuario.email = ?"
                    runDB $ rawSql sql2 [toPersistValue pid, toPersistValue logged] :: Handler [(Entity Pedido,Entity Servico,Entity Usuario,Entity Situacao)]
                else
                    runDB $ rawSql sql [toPersistValue pid] :: Handler [(Entity Pedido,Entity Servico,Entity Usuario,Entity Situacao)]  
            _ -> do
                runDB $ rawSql sql [toPersistValue pid] :: Handler [(Entity Pedido,Entity Servico,Entity Usuario,Entity Situacao)]  
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
        $(whamletFile "templates/requisicao.hamlet")
        $(whamletFile "templates/footer.hamlet")

getRequisicoesR :: Handler Html
getRequisicoesR = do
    logged <- lookupSession "_ID"
    let sql = "SELECT ??,??,??,?? FROM pedido \
      \ INNER JOIN usuario ON pedido.uid = usuario.id \
      \ INNER JOIN servico ON pedido.sid = servico.id \
      \ INNER JOIN situacao ON pedido.stid = situacao.id "
    pedidos <- do
        case logged of
            Just email -> do
                if (email /= "admin") then do
                    let sql2 = sql ++ " WHERE usuario.email = ?"
                    runDB $ rawSql sql2 [toPersistValue logged] :: Handler [(Entity Pedido,Entity Servico,Entity Usuario,Entity Situacao)]
                else
                    runDB $ rawSql sql [] :: Handler [(Entity Pedido,Entity Servico,Entity Usuario,Entity Situacao)]  
            _ -> do
                runDB $ rawSql sql [] :: Handler [(Entity Pedido,Entity Servico,Entity Usuario,Entity Situacao)]  
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
        $(whamletFile "templates/requisicoes.hamlet")
        $(whamletFile "templates/footer.hamlet")

formTrocarSituacao :: SituacaoId -> Pedido -> Form Pedido
formTrocarSituacao sid mc = renderDivs $ Pedido 
    <$> pure (pedidoUid mc)
    <*> pure (pedidoSid mc)
    <*> pure sid
    <*> pure (pedidoData mc)
    <*> pure (pedidoDescricao mc)

trocarSituacao :: SituacaoId -> PedidoId -> Text -> Handler Html
trocarSituacao sid pid situ = do
    pedido <- runDB $ get404 pid
    ((result, _), _) <- runFormPost (formTrocarSituacao sid pedido)
    case result of
        FormSuccess novoPedido -> do 
            runDB $ replace pid novoPedido
            setMessage [shamlet|
                    <p style="color: #673ab7;font-weight: bold;">
                        Requisição #{situ} com sucesso!
            |]
            redirect $ RequisicaoR pid
        _ -> do
            redirect RequisicoesR

getAprovarRequisicaoR :: PedidoId -> Handler Html
getAprovarRequisicaoR pid = do
    pedido <- runDB $ get404 pid
    (widget, _) <- generateFormPost (formTrocarSituacao (toSqlKey 2) pedido)
    msg <- getMessage -- Handler (Maybe Text)
    logged <- lookupSession "_ID"
    let sql = "SELECT ??,??,?? FROM pedido \
      \ INNER JOIN servico ON pedido.sid = servico.id \
      \ INNER JOIN usuario ON pedido.uid = usuario.id \
      \ WHERE pedido.id = ?"
    pedidos <- do
        case logged of
            Just email -> do
                if (email /= "admin") then do
                    let sql2 = sql ++ " AND usuario.email = ?"
                    runDB $ rawSql sql2 [toPersistValue pid, toPersistValue logged] :: Handler [(Entity Pedido,Entity Servico,Entity Usuario)]
                else
                    runDB $ rawSql sql [toPersistValue pid] :: Handler [(Entity Pedido,Entity Servico,Entity Usuario)]  
            _ -> do
                runDB $ rawSql sql [toPersistValue pid] :: Handler [(Entity Pedido,Entity Servico,Entity Usuario)]  
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
        $(whamletFile "templates/formAprovar.hamlet")
        $(whamletFile "templates/footer.hamlet")

getRejeitarRequisicaoR :: PedidoId -> Handler Html
getRejeitarRequisicaoR pid = do
    pedido <- runDB $ get404 pid
    (widget, _) <- generateFormPost (formTrocarSituacao (toSqlKey 3) pedido)
    msg <- getMessage -- Handler (Maybe Text)
    logged <- lookupSession "_ID"
    let sql = "SELECT ??,??,?? FROM pedido \
      \ INNER JOIN servico ON pedido.sid = servico.id \
      \ INNER JOIN usuario ON pedido.uid = usuario.id \
      \ WHERE pedido.id = ?"
    pedidos <- do
        case logged of
            Just email -> do
                if (email /= "admin") then do
                    let sql2 = sql ++ " AND usuario.email = ?"
                    runDB $ rawSql sql2 [toPersistValue pid, toPersistValue logged] :: Handler [(Entity Pedido,Entity Servico,Entity Usuario)]
                else
                    runDB $ rawSql sql [toPersistValue pid] :: Handler [(Entity Pedido,Entity Servico,Entity Usuario)]  
            _ -> do
                runDB $ rawSql sql [toPersistValue pid] :: Handler [(Entity Pedido,Entity Servico,Entity Usuario)]  
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
        $(whamletFile "templates/formRejeitar.hamlet")
        $(whamletFile "templates/footer.hamlet")

postAprovarRequisicaoR :: PedidoId -> Handler Html
postAprovarRequisicaoR pid = trocarSituacao (toSqlKey 2) pid "aprovada"

postRejeitarRequisicaoR :: PedidoId -> Handler Html
postRejeitarRequisicaoR pid = trocarSituacao (toSqlKey 3) pid "rejeitada"
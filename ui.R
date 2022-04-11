library(bs4Dash)
library(bslib)
library(fresh)
library(data.table)
library(shinyvalidate)
library(shinyalert)
useShinyalert(force = TRUE)
# library(fresh)
tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")
dashboardPage(skin = "light",
              
  dashboardHeader(skin = "light", title = title), 
  dashboardSidebar(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    sidebarMenu(
      menuItem("ACTIVIDADES", startExpanded = FALSE,
               menuSubItem("Execução", tabName = "awpb_status"),
               menuSubItem("PAAO", tabName = "awpb_updating")),
      
      menuItem("DESPESAS", startExpanded = FALSE,
               menuSubItem("Execução", tabName = 'expenditure_level'),
               menuSubItem("Pagamentos correntes", tabName = 'current_payments'),
               menuSubItem("Pagamento contratuais", tabName = 'contract_payments'),
               menuSubItem("Impressão de IP", tabName = 'imprimir_ip'),
               menuSubItem("Aprovações", tabName = 'expense_approvals')),
      
      menuItem("CONTRATAÇÕES", startExpanded = FALSE,
               menuSubItem("Execução", tabName = 'procurement_status'),
               menuSubItem("Dossiers", tabName = 'procurement_updates'),
               menuSubItem("Desempenho contratual", tabName = 'contract_performance'),
               menuSubItem("Actualizações Contratuais", tabName = 'contracts_updating'),
               menuSubItem("Balanços Contratuais", tabName = 'contract_ballance')),
      
      dateRangeInput("periodo_referencia", "Período:", start = "2022-01-01", end = "2021-12-31", min = "2020-05-22", max = "2030-12-31"),
      sliderInput("anos_referencia", "Anos", min = 2020, max = 2025, value = 2022, sep = ""),
      selectInput("responsible_technician", "Responsáveis Internos", choices = c("Todos", unique(procava_staff$awpb_role)), selected = "Todos"),
      selectInput("unidades_gestoras", "Unidades Gestoras", choices = c("Todas", management_units), selected = "Todas"),
      actionButton("aplicar_macrofiltros", "Aplicar Filtros!", width="87.5%"))),
  
  dashboardBody(tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tabItems(
      tabItem("awpb_updating", fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        div(style = "height:10px"),
        fluidRow(width="100%",
                column(width= 3,selectizeInput("responsaveis", "Responsáveis", multiple = FALSE, choices = c("Todos", responsaveis), selected = "Todos")),
                column(width= 2,selectizeInput("quadro_logico", "Quadro Lógico", choices = c("Todas", "Contribuição directa" = "NO", "Contribuição indirecta"= "YES"), selected = "Todas")),
                column(width= 2,selectizeInput("mader_relavant", "Prioridade MADER", choices = c("Todas", "Baixa Prioridade" = "NO", "Alta Prioridade"= "YES"), selected = "Todas")),
                column(width= 1,selectizeInput("mader_institutions", "Entidades", multiple = TRUE, choices = c("Todas", instituicoes), selected = "Todas")),
                column(width= 2,selectizeInput("relevance", "Nível da Actividade", choices = c("Todas", "Simples" = "Activity", "Compostas" = "Macro-activity", "Lotes" = "Lot", "Sub-actividades" = "Sub-activity"), selected = c("Macro-activity", "Activity"), multiple = TRUE)),
                column(width= 2, selectizeInput("critical_activity", "Importância", choices = c("Todas", "Não Crítica" = "Not Critical", "Crítica" = "Critical"), selected = "Todas")),
                column(width = 1, checkboxInput("apenas_gaza", "URGPS", value = FALSE)),
                column(width = 1, checkboxInput("apenas_niassa", "UPGPN", value = FALSE)),
                
                actionButton("recalcular_realizacoes","", icon=icon("calculator")),
                DT::dataTableOutput("responses_table"),
                tags$script("$(document).on('click', '#responses_table button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random())});")
        )
      )),
      
      tabItem("awpb_status", fluidRow(
          valueBoxOutput("actividades_iniciadas", width=3),
          valueBoxOutput("actividades_latentes",  width=3),
          valueBoxOutput("actividades_concluidas", width=3),
          valueBoxOutput("taxa_conclusoes", width=3),

          
        box(title = "Actividades planificadas vs iniciadas", closable = TRUE, width = 6, height = "600px", collapsible = TRUE,
             collapsed = FALSE, maximizable = TRUE, plotOutput("responsaveis_estado", height = "100%", width = "100%")),
        box(title = "Actividades iniciadas vs concluídas", closable = TRUE, width = 6, height = "600px", collapsible = TRUE,
            collapsed = FALSE, maximizable = TRUE, plotOutput("por_iniciar", height = "100%", width = "100%")),
        
        box(title = "EXECUÇÃO FÍSICA E FINANCEIRA GLOBAL", closable = TRUE, width = 12, height = "600px", collapsible = TRUE,
            collapsed = FALSE, maximizable = TRUE, echarts4rOutput("physical_execution", height = "100%", width = "100%")),
        
        box(title = "Estágios das actividades vs componente", closable = TRUE, width = 12, height = "550px", collapsible = TRUE,
            collapsed = TRUE, maximizable = TRUE, DTOutput("resumo_actividades", height = "100%", width = "100%")),
        box(title = "Impacto Orçamental das Actividades (US$)", closable = TRUE, width = 12, height = "550px", collapsible = TRUE,
            collapsed = TRUE, maximizable = TRUE, DTOutput("impacto_orcamental", height = "100%", width = "100%")),
      )),
      

      tabItem("expenditure_level", fluidRow(
        valueBoxOutput("montante_desembolsado", width=3),
        valueBoxOutput("cumulativo_executado",  width=3),
        valueBoxOutput("executado_rpsf", width=3),
        valueBoxOutput("comparticipacao_governo", width=3),
        
        box(title = "HISTÓRICO DE DESPESAS POR FINANCIADOR ('000 USD)", closable = TRUE, width = 6, height = "550px", collapsible = TRUE,
            collapsed = FALSE, maximizable = TRUE,  echarts4rOutput("expense_timeline", height = "100%", width = "100%")),
        
        box(title = "DESPESAS POR TRIMESTRE (MZN)", closable = TRUE, width = 6, height = "550px", collapsible = TRUE,
            collapsed = FALSE, maximizable = TRUE, echarts4rOutput("despesas_trimestrais", height = "100%", width = "100%")),
        
        box(title = "Execução por trimestre e componente", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
            collapsed = TRUE, maximizable = TRUE, DTOutput("components_quarters")),
        
        box(title = "Execução por trimestre e categoria", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
            collapsed = TRUE, maximizable = TRUE, DTOutput("category_quarters")),
        
        box(title = "Execução por trimestre e financiador", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
            collapsed = TRUE, maximizable = TRUE, DTOutput("financiers_quarters")),
        
        box(title = "Execução por trimestre e rubrica", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
            collapsed = TRUE, maximizable = TRUE, DTOutput("execucao_ced"))
      )),
      
      tabItem("current_payments", fluidRow(
        box(title = "PAGAMENTO DE DESPESAS CORRENTES", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
            collapsed = FALSE, maximizable = TRUE,
            fileInput('info_proposta', '', accept = ".xlsx"),
            actionButton("submeter_informacao_proposta", "SUBMETER", icon("save")),
            DT::dataTableOutput("informacao_proposta")))),
      
      tabItem("imprimir_ip", fluidRow(
        box(title = "IMPRESSÃO DE INFORMAÇÃO PROPOSTA", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
            collapsed = FALSE, maximizable = TRUE,
            fluidRow(
            column(width = 7, selectInput("descricao_processo", "Selecione o processo", choices = c("Todos", detailed), selected = "Todos", multiple = FALSE)),
            column(width = 3, selectInput("despesa_a_pagar", "Tipo de informação proposta", choices = c("Todas", tipos_de_despesas), selected = "Todas", multiple = FALSE)),
            column(width = 2, dateInput("data_formulacao", "Data de formulação"))),
            # actionButton("escrever_ip", "Baixar IP", icon("download"), style = "color: white; background-color: blue; "),
            tags$style(type="text/css", "#downloadData {background-color:blue;color: white;font-family: Courier New}"),
            downloadButton('downloadData', label = "Baixar IP"),
            br(),
            div(style = "height:10px"),
            DT::dataTableOutput("dados_processo")))
           ),
  
      tabItem("contract_payments", fluidPage(surveyOutput(df = df, survey_title = "PAGAMENTO DE CONTRATOS", theme = "#d5f97c"))),
      # tabItem("procurement_status", fluidPage(surveyOutput(contractrecords_df, survey_title = "REGISTO INICIAL DE CONTRATOS"))),
      tabItem("expense_approvals", fluidRow(
        tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.css"),
                  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js")),
        
        box(title = "APROVAÇÃO DE PAGAMENTOS", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
            actionButton("saveBtn","SALVAR"),
            div(style = "height:10px"),
            DT::dataTableOutput("payment_approvals"))
      )),
      
      tabItem("procurement_status", fluidRow(
        valueBoxOutput("dossiers_iniciados", width=3),
        valueBoxOutput("dossiers_contratados",  width=3),
        valueBoxOutput("usd_em_procurement", width=3),
        valueBoxOutput("usd_latentes", width=3),
        
        box(title = "SITUAÇÃO POR RESPONSÁVEL ('000 USD)", closable = TRUE, width = 6, height = "550px", collapsible = TRUE, collapsed = FALSE, maximizable = TRUE,
            plotOutput("situacao_pp_responsaveis", height = "100%", width = "100%")),
        
        box(title = "DOSSIERS LATENTES POR RESPONSÁVEL ('000 USD)", closable = TRUE, width = 6, height = "550px", collapsible = TRUE, collapsed = FALSE, maximizable = TRUE,
            plotOutput("dossiers_latentes_responsavel", height = "100%", width = "100%")),
        
        box(title = "DOSSIERS POR COMPONENTE E SUBCOMPONENTE ('000 USD)", closable = TRUE, width = 12, height = "550px", collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
            DT::dataTableOutput("dossiers_components_usd")),
        
        box(title = "DOSSIERS POR COMPONENTE E SUBCOMPONENTE", closable = TRUE, width = 12, height = "550px", collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
            DT::dataTableOutput("dossiers_components")),
        
        box(title = "DOSSIERS POR CATEGORIA E MÉTODO ('000 USD)", closable = TRUE, width = 12, height = "550px", collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
            DT::dataTableOutput("dossiers_methods_usd")),
        
        box(title = "DOSSIERS POR CATEGORIA E MÉTODO", closable = TRUE, width = 12, height = "550px", collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
            DT::dataTableOutput("dossiers_methods")),
        
      )),
      
      
      tabItem("contract_performance", fluidRow(
        valueBoxOutput("contratos_celebrados", width=3),
        valueBoxOutput("usd_contratados",  width=3),
        valueBoxOutput("usd_pagos", width=3),
        valueBoxOutput("contratos_fechados", width=3),
        
        box(title = "# DE CONTRATOS CELEBRADOS VS CONCLUÍDOS", closable = TRUE, width = 6, height = "550px", collapsible = TRUE, collapsed = FALSE, maximizable = TRUE,
            plotOutput("signed_contracts", height = "100%", width = "100%")),
        
        box(title = "CONTRATOS ASSINADOS VS CONCLUÍDOS ('000 USD)", closable = TRUE, width = 6, height = "550px", collapsible = TRUE, collapsed = FALSE, maximizable = TRUE,
            plotOutput("ammount_contracted", height = "100%", width = "100%")),
        
        box(title = "SITUAÇÃO DOS CONTRATOS POR COMPONENTE ('000 USD)", closable = TRUE, width = 12, height = "550px", collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
            DT::dataTableOutput("component_contracts")),
        
        box(title = "CATEGORIAS DOS CONTRATOS ('000 USD)", closable = TRUE, width = 12, height = "550px", collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
            fluidRow(
            column(width= 4,selectInput("linhas_contratos", "Variável das linhas", choices = variaveis_contratos, selected = "contract_status")),
            column(width=4,selectInput("colunas_contratos", "Variável das colunas", choices = variaveis_contratos, selected = "procurement_type")),
            column(width=4,selectInput("numeric_contratos", "Valor", choices = valores_contratos, selected = "revised_ammount")),
            br(),
            DT::dataTableOutput("contract_categories")))
        
      )),
      
      tabItem("procurement_updates", fluidPage(
        
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        fluidRow(actionButton("edit_dossier", "ACTUALIZAR", icon("edit"))),
        div(style = "height:10px"),
        fluidRow(width="100%",
                 column(width= 4,selectInput("dossier_responsibles", "Responsável", multiple = TRUE, choices = c("Todos", pp_responsibles), selected = "Todos")),
                 column(width= 4,selectInput("procurement_sheets", "Categoria de Procurement", multiple = TRUE, choices = c("Todos", "Consultoria" = "CONSULTANCY", "Bens" = "GOODS","Construções" = "WORKS"), selected = "Todos")),
                 column(width= 4,selectInput("procurement_methods", "Métodos", multiple = TRUE, choices = c("Todos", unique(procurement_view$method_name_pt), selected = "Todos"))),
                 
                 div(style = "height:10px"),
                 DT::dataTableOutput("ppdossiers")
        )
      )),
      
      #######################  ESTA ABA DEVE FICAR ESCONDIDA  ###########################
      tabItem("contracts_updating", fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        fluidRow(actionButton("edit_aggreements", "ACTUALIZAR", icon("edit"))),
        br(),
        fluidRow(width="100%", selectInput("gestores_contractuais", "Gestor do Contrato", multiple = FALSE, choices = c("Todos", staff_choices), selected = "Todos"),
                 br(),
                 DT::dataTableOutput("contractdossiers")
        )
      )),
      
      tabItem("contract_ballance", fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        # fluidRow(actionButton("edit_aggreements", "ACTUALIZAR", icon("edit"))),
        # br(),
        fluidRow(width="100%", 
                 column(width = 4, selectizeInput("gestor_do_contrato", "Gestor do Contrato", multiple = FALSE, choices = c("Todos", staff_choices), selected = "Todos", width="350px")),
                 column(width = 6, selectizeInput("numerador_contratual", "Número do Contrato", multiple = FALSE, choices = c("Todos", contract_numbers), selected = "Todos", width="600px")),
                 column(width = 2, selectizeInput("situacao_contratual", "Situação do Contrato", multiple = FALSE, choices = c("Todos", contract_status), selected = "Todos")),
                 # contract_status
                 div(style = "height:10px"),
                 DT::dataTableOutput("contract_execution_status")
        )
      ))
    )
  )
  
)

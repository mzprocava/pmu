library(bs4Dash)
library(DT)
library(pool)
library(dplyr)
library(tidyverse)
library(DBI)
library(RPostgres)
library(ggcharts)
library(ggplot2)
library(echarts4r)
library(janitor)
library(lubridate)
library(feather)
library(fst)
library(shinysurveys)
library(shinyvalidate)
library(extrafont)
library(showtext)
library(shinyjs)
library(sodium)
library(httr)
library(bslib)
library(fresh)
library(splitstackshape)
library(zoo)
library(data.table)
library(flextable)
library(glue)
library(shinyFiles)
library(readr)
library(readxl)
library(officer)
library(officedown)
library(shinyFeedback)
library(shinyalert)
library(lares) ### for translation of columns
library(uuid)


bs4dash_font(size_base = "1.5rem", weight_bold = 900)
thematic::thematic_shiny(font = "auto")
options(scipen = 9999)
options(digits=15)
options(warn = 0)
e_rate = 64.46

function(input, output, session) {
############################################### REACTIVE DATASETS #############################
  filtered_awpb <- reactive({
    awpb_for_summary <- dbGetQuery(pool, "SELECT * FROM procava.awpb_for_summary")
    if(input$responsaveis != "Todos"){awpb_for_summary <- awpb_for_summary[awpb_for_summary$internal_responsible %in% input$responsaveis,]}
    if(input$mader_relavant != "Todas"){awpb_for_summary <- awpb_for_summary[awpb_for_summary$mader %in% input$mader_relavant,]}
    if(input$relevance != "Todas"){awpb_for_summary <- awpb_for_summary[awpb_for_summary$relevance %in% input$relevance,]}
    if(input$critical_activity != "Todas"){awpb_for_summary <- awpb_for_summary[awpb_for_summary$critical_path %in% input$critical_activity,]}
    if(input$mader_institutions != "Todas"){awpb_for_summary <- awpb_for_summary[awpb_for_summary$institution %in% input$mader_institutions,]}
    if(input$responsible_technician != "Todos"){awpb_for_summary <- awpb_for_summary[awpb_for_summary$internal_responsible %in% input$responsible_technician,]}
    if(input$apenas_niassa == TRUE){awpb_for_summary <- awpb_for_summary %>% dplyr::filter(niassa > 0) %>% dplyr::select(awpb_id, descricao_da_actividade, unidades, niassa_usd, niassa, q1_niassa, q2_niassa, q3_niassa, q4_niassa, area, distrital_niassa, achieved_niassa, niassa_period_achievement, niassa_year_achievement, ammount_spent, financial_execution, chart_status, comentarios, internal_responsible, institution, relevance, critical_path, mader, pt_status, situacao, target, q1, q2, q3, q4, q1_south, q2_south, q3_south, q4_south, simplified_status, en_status, componentnum_pt, components_pt, province_locations, distrital_sul, achieved_sul, distrital_locations, month_achieved, achieved_national, srpmu_usd, current_year_budget_us, sul_year_achievement, sul_period_achievement, period_achievement, year_achievement, target_sul)}
    if(input$apenas_gaza == TRUE){awpb_for_summary <- awpb_for_summary %>% dplyr::filter(target_sul > 0) %>% dplyr::select(awpb_id, descricao_da_actividade, unidades, srpmu_usd, target_sul, q1_south, q2_south, q3_south, q4_south, area, distrital_niassa, achieved_niassa, sul_period_achievement, sul_year_achievement, ammount_spent, financial_execution, chart_status, comentarios, internal_responsible, institution, relevance, critical_path, mader, pt_status, situacao, target, q1, q2, q3, q4, q1_niassa, q2_niassa, q3_niassa, q4_niassa, simplified_status, en_status, componentnum_pt, components_pt, province_locations, distrital_sul, achieved_sul, distrital_locations, month_achieved, achieved_national, current_year_budget_us, niassa_period_achievement, niassa_year_achievement, period_achievement, year_achievement, niassa_usd, niassa)}
    
    awpb_for_summary
  })
  
  filtered_aggreements <- reactive({
    procava_aggreements <- DBI::dbGetQuery(pool, "SELECT supplier_name, contract_number, sign_date, revised_end, contractdescription_pt, currency, revised_ammount, contract_status, physical_compl, comments, contractdescription_en, no_number, supplier_address, supplier_country, supplier_city, supplier_email, supplier_phone, contract_manager, start_date, end_date, ammount, exchange_rate, revision_date, ifadgrant_percent, ifadloan_percent, rpsf1_percent, rpsf2_percent, government_percent, gcf_percent, beneficiary_percent, others_percent, perfguar_numb, perfguar_issuedate, perfguar_expdate, perfguar_issuer, perfguar_type, perfguar_value, advance_date, advguar_numb, advguar_issuer, advguar_issuedate, advguar_expdate, advguar_type, advguar_value, update_date, contract_perform, risk_flag, ammount_advanced FROM fiduciary.aggreements WHERE staff_contract = 'NO'")
    if(input$gestores_contractuais !="Todos"){procava_aggreements <- procava_aggreements[procava_aggreements$contract_manager %in% input$gestores_contractuais,]}
    procava_aggreements
  })
  
  contract_ballance <- reactive({
    contract_ballances <- DBI::dbGetQuery(pool, "SELECT * FROM fiduciary.external_contract_ballance")
    if(input$gestor_do_contrato !="Todos"){contract_ballances <- contract_ballances[contract_ballances$contract_manager %in% input$gestor_do_contrato,]}
    if(input$numerador_contratual !="Todos"){contract_ballances <- contract_ballances[contract_ballances$contract_number %in% input$numerador_contratual,]}
    if(input$situacao_contratual !="Todos"){contract_ballances <- contract_ballances[contract_ballances$contract_status %in% input$situacao_contratual,]}
    contract_ballances
  })

  
  long_aggreements <- reactive({
    long_aggreements <- DBI::dbGetQuery(pool, "SELECT * FROM fiduciary.long_aggreement")
    if(input$gestores_contractuais !="Todos"){long_aggreements <- long_aggreements[long_aggreements$contract_manager %in% input$gestores_contractuais,]}
    # if(input$unidades_gestoras !="Todas"){long_aggreements <- long_aggreements[long_aggreements$cost_center %in% input$unidades_gestoras,]}
    long_aggreements
  })

  filtered_dossiers <-reactive({
    ppdossiers <- DBI::dbGetQuery(pool, "SELECT activ_id, idpp, activity_description_pt,  method_name_pt,  basic_usd, responsible, procurement_stage, comments_update, eo_i_submission, no_eo_i, inv_r_eo_i, 
                  close_r_eo_i, report_eo_i, no_eo_i_report, rfp_rno, rfp_no, invitation, closing, rno_eva_r, no_eva_r, 
                  full_eva_rno, full_eva_no, noita, contract_awards, negotiations, rno_contract, no_contract, signature, envelopes, 
                  contract_n, vendor_id, contract_usd, contract_completion, lot,  ifadpp_sheet, ifad_review, qualification, proc_methods,
                  non_consulting, activity_pt, lotes, is_grant, shortlisting_pp, non_consultancies, activity, simplexstage_en, components_pt, componentnum_pt from fiduciary.procurement_view")
    if(input$dossier_responsibles !="Todos"){ppdossiers <- ppdossiers[ppdossiers$responsible %in% input$dossier_responsibles,]}
    ppdossiers
  })

  paao_granulado <-reactive({
    paao_granulado <- read_feather('granular_awpb_2022.feather')
    if(input$responsible_technician !="Todos"){paao_granulado <- paao_granulado[paao_granulado$responsibility %in% input$responsible_technician,]}
    paao_granulado
  })
  
  full_approved_payments <-reactive({
    full_approved_payments <- dbGetQuery(pool, "SELECT * from fiduciary.full_approved_payments")
    if(input$unidades_gestoras !="Todas"){full_approved_payments <- full_approved_payments[full_approved_payments$cost_centre %in% input$unidades_gestoras,]}
    full_approved_payments
  })

  
  proposta_de_pagamento <-reactive({
    full_approved_payments <- dbGetQuery(pool, "SELECT * from fiduciary.full_payments_dataset")
    if(input$despesa_a_pagar !="Todas"){full_approved_payments <- full_approved_payments[full_approved_payments$tipos_de_despesas %in% input$despesa_a_pagar,]}
    if(input$descricao_processo !="Todos"){full_approved_payments <- full_approved_payments[full_approved_payments$detailed %in% input$descricao_processo,]}
    full_approved_payments
  })

  # ##################### AWPB VALUE BOXES  ######################
  output$actividades_iniciadas  <- renderValueBox({
    started <-  filtered_awpb() %>% select(situacao, awpb_id) %>% dplyr::filter(situacao %in% c("Iniciada (Execução < 50%)", "Estado avançado (50% < Execução < 75%)",
                                                                                            "Quase concluída (75% < Execução < 100%)", "Concluída  (Execução >= 100%)"))
    
    valueBox(tags$b(prettyNum(n_distinct(started$awpb_id)), big.mark=",",
                    style = "font-size: 400%;"),"Actividades iniciadas",icon = ionicon(name ="walk"), color = "indigo")
  })
  
  output$actividades_latentes   <- renderValueBox({
    started <-  filtered_awpb() %>% select(situacao, awpb_id) %>% dplyr::filter(situacao %in% c("Não iniciada (atrasada)", "Não iniciada (dentro do prazo)"))
    
    valueBox(tags$b(prettyNum(n_distinct(started$awpb_id)), big.mark=",",
                    style = "font-size: 400%;"),"Actividades não iniciadas",icon = ionicon(name ="warning"), color = "danger")
  })
  
  output$actividades_concluidas   <- renderValueBox({
    started <- filtered_awpb() %>% select(situacao, awpb_id) %>% dplyr::filter(situacao %in% c("Concluída  (Execução >= 100%)"))
    
    valueBox(tags$b(prettyNum(n_distinct(started$awpb_id)), big.mark=",",
                    style = "font-size: 400%;"),"Actividades Concluídas",icon = icon("calendar-check"), color = "success")
  })
 
  output$taxa_conclusoes   <- renderValueBox({
    started <-  filtered_awpb() %>% select(situacao, awpb_id) %>% dplyr::filter(situacao %in% c("Concluída  (Execução >= 100%)"))
    valor <- paste(round(n_distinct(started$awpb_id)/n_distinct(filtered_awpb()$awpb_id)*100, 2),"%")
    valueBox(tags$b(valor, style = "font-size: 400%;"),"Nível de Conclusão das Actividades",icon = icon("percent"), color = "info")
  })
  
  #################  AWPB CHARTS ##################
  
  output$responsaveis_estado <- renderPlot({
    finalizadas <- filtered_awpb() %>% group_by(internal_responsible, simplified_status) %>% summarize(n_distinct(awpb_id)) %>% spread(simplified_status, -internal_responsible) %>% adorn_totals("col")
    finalizadas$Iniciadas <- finalizadas$Total - finalizadas$`Latente`
    finalizadas[is.na(finalizadas)] <- 0
    finalizadas$internal_responsible <- fct_reorder(finalizadas$internal_responsible, finalizadas$Total, min)
    ggplot(finalizadas, aes(x= internal_responsible, y = Total)) + geom_bar(stat= "identity", col = "#c4fb83",fill="#c4fb83") +
      geom_bar(aes(x= internal_responsible, y = Iniciadas), stat= "identity", width = 0.5,col = "#10b005",fill="#10b005") + theme_void() +
      labs(x = "", y = "# de actividades", caption = "Fonte: Actualizações do PAAO @ FAR,FP - PROCAVA")+
      theme(axis.text.y = element_text(size = 10))+
      geom_text(aes(label=round(Total), hjust = 1.5), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
      coord_flip()
  })
  
  output$por_iniciar <- renderPlot({
    finalizadas <- filtered_awpb() %>% group_by(internal_responsible, simplified_status) %>% summarize(n_distinct(awpb_id)) %>% spread(simplified_status, -internal_responsible) %>% adorn_totals("col")
    finalizadas[is.na(finalizadas)] <- 0
    finalizadas$Iniciadas <- finalizadas$Total - finalizadas$Latente
    finalizadas$internal_responsible <- fct_reorder(finalizadas$internal_responsible, finalizadas$Iniciadas, min)
    
    ggplot(finalizadas, aes(x = internal_responsible, y = Iniciadas)) + geom_bar(stat= "identity", col = "#c4fb83",fill="#c4fb83") +
      geom_bar(aes(x= internal_responsible, y = Finalizada), stat= "identity", width = 0.5,col = "#10b005",fill="#10b005") + theme_void() +
      labs(x = "", y = "# de actividades", caption = "Fonte: Actualizações do PAAO @ FAR,FP - PROCAVA")+
      theme(axis.text.y = element_text(size = 10))+
      geom_text(aes(label=round(Iniciadas), hjust = 1.5), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
      coord_flip()
  })
  
  output$resumo_actividades <- DT::renderDataTable({
    totais <- filtered_awpb() %>% group_by(chart_status) %>% summarize(actividades = n_distinct(awpb_id))%>%
      pivot_wider(names_from = "chart_status", values_from = "actividades", values_fill = 0)
    tt_pct <- totais
    tt_pct <- tt_pct[,1:ncol(tt_pct)]/n_distinct(filtered_awpb()$awpb_id)*100
    tt_pct$components_pt <- "(%)"
    
    completions <- filtered_awpb() %>% group_by(components_pt, componentnum_pt, chart_status) %>% summarize(actividades = n_distinct(awpb_id)) %>%
      pivot_wider(names_from = "chart_status", values_from = "actividades", values_fill = 0)
    
    C1 <- completions %>% dplyr::filter(componentnum_pt == "Componente 1") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 1")) %>% arrange(components_pt)
    C2 <- completions %>% dplyr::filter(componentnum_pt == "Componente 2") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 2"))%>% arrange(components_pt)
    C3 <- completions %>% dplyr::filter(componentnum_pt == "Componente 3") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 3"))%>% arrange(components_pt)
    C4 <- completions %>% dplyr::filter(componentnum_pt == "Componente 4") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 4"))%>% arrange(components_pt)
    
    completions <- dplyr::bind_rows(C1, C2, C3, C4, totais, tt_pct)
    completions <- completions %>% dplyr::select(-2) %>% adorn_totals("col")
    completions$components_pt[is.na(completions$components_pt)] <- "Total"
    completions$col_share <- round(completions$Total/n_distinct(filtered_awpb()$awpb_id)*100,2)
    
    datatable(completions, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")
    
  })

  output$physical_execution <- renderEcharts4r({
    e_charts() |> 
      e_gauge(5.55, "Física (%)") |> 
      e_title("")
  })
  
  output$financial_execution <- renderEcharts4r({
    e_charts() |> 
      e_gauge(5.28, "Financeira (%)") |> 
      e_title("")
  })
  
  output$impacto_orcamental <- DT::renderDataTable({
    totais <- completions <- filtered_awpb() %>% group_by(chart_status) %>% summarize(actividades = sum(current_year_budget_us))%>%
      pivot_wider(names_from = "chart_status", values_from = "actividades", values_fill = 0)
    tt_pct <- totais
    tt_pct <- tt_pct[,1:ncol(tt_pct)]/sum(filtered_awpb()$current_year_budget_us)*100
    tt_pct$components_pt <- "(%)"
    
    completions <- filtered_awpb()%>% group_by(components_pt, componentnum_pt, chart_status) %>% summarize(actividades = sum(current_year_budget_us)) %>%
      pivot_wider(names_from = "chart_status", values_from = "actividades", values_fill = 0)
    
    C1 <- completions %>% dplyr::filter(componentnum_pt == "Componente 1") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 1")) %>% arrange(components_pt)
    C2 <- completions %>% dplyr::filter(componentnum_pt == "Componente 2") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 2"))%>% arrange(components_pt)
    C3 <- completions %>% dplyr::filter(componentnum_pt == "Componente 3") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 3"))%>% arrange(components_pt)
    C4 <- completions %>% dplyr::filter(componentnum_pt == "Componente 4") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 4"))%>% arrange(components_pt)
    
    completions <- dplyr::bind_rows(C1, C2, C3, C4, totais, tt_pct)
    completions <- completions %>% dplyr::select(-2) %>% adorn_totals("col")
    completions$components_pt[is.na(completions$components_pt)] <- "Total"
    completions$col_share <- round(completions$Total/sum(filtered_awpb()$current_year_budget_us)*100,2)
    
    datatable(completions, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")
  })

  output$responses_table <- DT::renderDataTable({
    DT  <- as.data.frame(filtered_awpb())
    DT$period_achievement[DT$period_achievement == 999] <- NA
    DT$year_achievement[DT$year_achievement == 999] <- NA
    DT$niassa_period_achievement[DT$niassa_period_achievement == 999] <- NA
    DT$sul_period_achievement[DT$sul_period_achievement == 999] <- NA
    DT$niassa_year_achievement[DT$niassa_year_achievement == 999] <- NA
    DT$sul_year_achievement[DT$sul_year_achievement == 999] <- NA
    
    # DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
    DT[["Actions"]]<-
      paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
                <button type="button" class="fa fa-edit modify" style="color:blue" id=modify_',1:nrow(DT),'></button>
             </div>
             ')
    DT[["IP"]]<-
      paste0('
             <div class="btn-group" role="group" aria-label="Proposta">
                <button type="button" class="fa fa-coins execute" style="color:blue" id=execute_',1:nrow(DT),'></button>
             </div>
             ')
    
    DT <- DT %>% select(Actions, IP, everything())

    names(DT) <- c('','IP', 'Ref.ª', 'Actividade', 'Unidade', 'Orçamento (US$)', 'Meta', 'Q1', 'Q2', 'Q3', 'Q4', 'Abrangência', 'Distritos Real', 'Real', 'Período (%)', 'Ano (%)', 'Despesas (MZN)', '(%)', 'Situação', 'Comentários', 'internal_responsible', 'institution', 'relevance', 'critical_path', 'mader', 'pt_status', 'situacao', 'target', 'q1', 'q2', 'q3', 'q4', 'q1_niassa', 'q2_niassa', 'q3_niassa', 'q4_niassa', 'simplified_status', 'en_status', 'componentnum_pt', 'components_pt', 'province_locations', 'distrital_sul', 'achieved_sul', 'distrital_locations', 'month_achieved', 'achieved_national', 'current_year_budget_us', 'sul_year_achievement', 'sul_period_achievement', 'period_achievement', 'year_achievement', 'niassa_usd', 'niassa')
    
    datatable(DT, escape = FALSE,  rownames=F, extensions = c('Buttons'), options = list(dom = 'Bfrtip', buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print'),  pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
                                                                                         columnDefs = list(list(targets = c(20:(ncol(DT)-1)), visible = FALSE))))%>%
      
      formatStyle(columns = c('Situação'), backgroundColor = styleEqual(c("Atrasada", "Latente", "Iniciada",
                                                                          "Quase a terminar", "Fase final",
                                                                          "Terminada"), c("#f15a4e", "#f9ad6e","#fced03", "#8cf827", "#05b205","#00FF00"))) %>%
      
      formatRound(c('Orçamento (US$)','Meta', 'Q1', 'Q2','Q3','Q4'), digits = 0) %>% 
      formatRound(c('Real', 'Período (%)', 'Ano (%)'), digits = 2) %>% 
      formatStyle(columns = c('Q1', 'Q2', 'Q3', 'Q4'), backgroundColor = styleInterval(cuts =c(0,1), values = c("#f5f7f7", "none","#00f0ff")), fontWeight = styleInterval(cuts = 1,values = c("normal","bold"))) %>% 
      # formatStyle(columns = c(12), backgroundColor = styleInterval(cuts =c(0,25,50,75,100,998), values = c("#f15a4e", "#f9ad6e","#fced03", "#8cf827","#05b205","#d0d5d0")), fontWeight = styleInterval(cuts = 6,values = c("normal","bold", "normal","bold", "normal","bold"))) %>% 
      formatStyle(columns = c('Período (%)', 'Ano (%)'), backgroundColor = styleInterval(cuts =c(0,25,50,75,100,998), values = c("#f15a4e", "#f9ad6e","#fced03", "#8cf827", "#05b205","#05b205","#d0d5d0")), fontWeight = styleInterval(cuts = 1,values = c("bold","normal"))) %>% 
      
      formatCurrency(c('Orçamento (US$)', "Despesas (MZN)", "(%)"), '') %>% 
      formatStyle(c('Situação', 'Abrangência', 'Unidade'), `text-align` = 'center')
  })
  
  
  observeEvent(input$lastClick, {if (input$lastClickId%like%"modify") {showModal(awpb_update_form)}
                 else if (input$lastClickId%like%"execute") {showModal(payments_form)}})
  
  fieldsMandatory <- c("situacao", "comentarios")
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    shinyjs::toggleState(id = "update_awpb", condition = mandatoryFilled)
  })

  ############# FINANCE OUTPUTS #################
  output$expense_timeline <- renderEcharts4r({
    
    pagamentos <- dbGetQuery(pool, "SELECT * from fiduciary.full_approved_payments")
    pagamentos$payment_date <- if_else(pagamentos$payment_date == as.Date("1899-12-30"), as.Date("2020-12-24"), pagamentos$payment_date)
    pagamentos$month <- toupper(months(ymd(pagamentos$payment_date), abbreviate = TRUE))
    pagamentos$month_end <- ceiling_date(pagamentos$payment_date, "month") - 1
    
    pagamentos_mensais <- pagamentos %>% group_by(funding, payment_date) %>% summarize(despesas = round(sum(paid_ammount)/1000),2)
    ts_base <- pagamentos_mensais %>%
      e_charts(x = payment_date) %>%
      e_datazoom(
        type = "slider",
        toolbox = FALSE,
        bottom = -5
      ) %>%
      e_tooltip() %>%
      e_scatter(payment_date, despesas) |>
      e_title("") %>%
      e_x_axis(payment_date, axisPointer = list(show = TRUE))
    ts_base %>% e_line(despesas)
  })
  #
  output$despesas_trimestrais <- renderEcharts4r({
    budgeted <- read_feather('granular_awpb_2022.feather')
    executed <- dbGetQuery(pool, "SELECT * FROM fiduciary.full_approved_payments")
    executed$quarters <- quarters(executed$submission_date)
    budgeted_quarters <- budgeted %>% group_by(fiscal_quarters) %>% summarise(Plan = round(sum(granularusd)),2)
    executed_quarters <-   executed %>% group_by(quarters) %>% summarise(Actuals = round(sum(paid_ammount)),2)
    plan_vs_actuals <- merge(budgeted_quarters, executed_quarters, by.x = "fiscal_quarters", by.y = "quarters", all = TRUE)
    plan_vs_actuals[is.na(plan_vs_actuals)] <- 0
    
    plan_vs_actuals %>%
      e_charts(fiscal_quarters) %>%
      e_bar(`Plan`) %>%
      e_bar(`Actuals`) %>%
      e_tooltip(trigger = "item")
  })
  #
  output$execucao_componente <- DT::renderDataTable({
    components_paid <- paid_set %>% group_by(component, subcomponent,quarters) %>% summarize(montante = sum(paid_ammount)) %>% pivot_wider(names_from = quarters, values_from = montante)
    components_paid <- components_paid %>% dplyr::arrange(component) %>% split( .[,"component"] ) %>% purrr::map_df(., janitor::adorn_totals)
    components_paid$subcomponent <- ifelse(components_paid$subcomponent == "-", lag(components_paid$component),components_paid$subcomponent)
    components_paid_TT <- paid_set %>% group_by(quarters) %>% summarize(montante = sum(paid_ammount)) %>% pivot_wider(names_from = quarters, values_from = montante)
    components_paid <- dplyr::bind_rows(components_paid, components_paid_TT)
    components_paid$subcomponent[is.na(components_paid$subcomponent)] <- "Total"
    components_paid <- components_paid %>% adorn_totals("col")
    components_planned <- granular_awpb_2022 %>% group_by(component, subcomponent,fiscal_quarters) %>% summarize(montante = sum(granularmzn)) %>% pivot_wider(names_from = fiscal_quarters, values_from = montante)
    components_planned <- components_planned %>% dplyr::arrange(component) %>% split( .[,"component"] ) %>% purrr::map_df(., janitor::adorn_totals)
    components_planned$subcomponent <- ifelse(components_planned$subcomponent == "-", lag(components_planned$component),components_planned$subcomponent)
    components_planned_TT <- granular_awpb_2022 %>% group_by(fiscal_quarters) %>% summarize(montante = sum(granularmzn)) %>% pivot_wider(names_from = fiscal_quarters, values_from = montante)
    components_planned <- dplyr::bind_rows(components_planned, components_planned_TT) %>% adorn_totals("col")
    components_planned$subcomponent[is.na(components_planned$subcomponent)] <- "Total"
    components_planned <- components_planned %>% select(subcomponent, AWPB = Total)
    component_quarters <- merge(components_paid, components_planned, by = "subcomponent", all= TRUE) %>% mutate(Execution = Total/AWPB*100)
    component_quarters <- merge(component_quarters, components_design, by = "subcomponent", all = TRUE) %>% mutate(Expenditure = Total/PDR/64.46*100)
    component_quarters$copy <- component_quarters$subcomponent
    component_quarters <- component_quarters %>% arrange(copy) %>% dplyr::select(-copy, -component)
    datatable(component_quarters, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'tB', buttons = c('copy', 'csv', 'excel')), class = "display")
    
  })
  #
  output$execucao_categoria <- DT::renderDataTable({
    paid_categories <- paid_set
    paid_categories$pdr_category[paid_categories$pdr_category =="Operating costs"] <- "Operating Costs"
    
    paid_categories <- paid_categories %>% group_by(cost_centre, pdr_category) %>% summarise(soma_paga = sum(paid_ammount))  %>%
      pivot_wider(names_from =  "cost_centre", values_from = "soma_paga") %>%
      adorn_totals("col")
    
    paid_categories$pdr_category[paid_categories$pdr_category == "Works"] <- "Civil Works"
    paid_categories$pdr_category[paid_categories$pdr_category == "Salaries and allowances"] <- "Salaries and Allowances"
    
    planned_categories <- granular_awpb_2022 %>% group_by(pdr_category) %>% summarise(AWPB = sum(granularmzn))
    paid_categories <- merge(paid_categories, planned_categories, by = "pdr_category", all = TRUE)
    paid_categories <- merge(paid_categories, PDR_categories, by = "pdr_category", all = TRUE)
    paid_categories <- paid_categories %>% adorn_totals("row")
    paid_categories$AWPB_percent <- paid_categories$Total/paid_categories$AWPB*100
    paid_categories$PDR_percent <- paid_categories$Total/paid_categories$PDR*100/64.46
    datatable(paid_categories, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")
  })
  #
  output$execucao_financiador <- DT::renderDataTable({
    paid_financiers <- paid_set
    paid_financiers <- paid_financiers %>% group_by(funding, quarters) %>% summarise(soma_paga = sum(paid_ammount))  %>%
      pivot_wider(names_from =  "quarters", values_from = "soma_paga") %>%
      adorn_totals("col")
    PDR_financiers <- cost_tabs_pdr %>% dplyr::filter(class == "financiers") %>% select(funding = Expenditure, PDR = total_cost)
    planned_financiers <- granular_awpb_2022 %>% group_by(financiers) %>% summarise(AWPB = sum(granularmzn))
    paid_financiers <- merge(paid_financiers, planned_financiers, by.x = "funding", by.y= "financiers", all = TRUE)
    paid_financiers <- merge(paid_financiers, PDR_financiers, by = "funding", all = TRUE)
    paid_financiers <- paid_financiers %>% adorn_totals("row")
    paid_financiers$AWPB_percent <- paid_financiers$Total/paid_financiers$AWPB*100
    paid_financiers$PDR_percent <- paid_financiers$Total/paid_financiers$PDR*100/64.46
    
    datatable(paid_financiers, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")
  })
  #
  output$execucao_financiador <- DT::renderDataTable({
    paid_financiers <- paid_set
    paid_financiers <- paid_financiers %>% group_by(funding, quarters) %>% summarise(soma_paga = sum(paid_ammount))  %>%
      pivot_wider(names_from =  "quarters", values_from = "soma_paga") %>%
      adorn_totals("col")
    PDR_financiers <- cost_tabs_pdr %>% dplyr::filter(class == "financiers") %>% select(funding = Expenditure, PDR = total_cost)
    planned_financiers <- granular_awpb_2022 %>% group_by(financiers) %>% summarise(AWPB = sum(granularmzn))
    paid_financiers <- merge(paid_financiers, planned_financiers, by.x = "funding", by.y= "financiers", all = TRUE)
    paid_financiers <- merge(paid_financiers, PDR_financiers, by = "funding", all = TRUE)
    paid_financiers <- paid_financiers %>% adorn_totals("row")
    paid_financiers$AWPB_percent <- paid_financiers$Total/paid_financiers$AWPB*100
    paid_financiers$PDR_percent <- paid_financiers$Total/paid_financiers$PDR*100/64.46
    
    
    datatable(paid_financiers, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")
  })

  output$informacao_proposta <- DT::renderDataTable({
    req(input$info_proposta)
    inFile <- input$info_proposta
    ficheiro <- read_excel(inFile$datapath, sheet = "DATASET")
    ficheiro$paid_ammount <-  as.numeric(ficheiro$paid_ammount)
    payments_dataset <- ficheiro %>% dplyr::filter(!is.na(paid_ammount))
    ficheiro_visivel <- payments_dataset
    ficheiro_visivel <- ficheiro_visivel %>% dplyr::select(beneficiary, awpb_id, processo_numero, detailed, payment_details, paid_ammount, contract,
                                                           e_sistafe_w_code, funding, applicant_name, everything())
    datatable(ficheiro_visivel, options=list(columnDefs = list(list(visible=FALSE, targets=c(13:ncol(ficheiro_visivel)))), rowsGroup = list(2)))
    
  })

  #  #####################  FINANCE VALUE BOXES
   output$montante_desembolsado  <- renderValueBox({
     valor <- paste0(round(sum(disbursed$contravalor_mzn)/1000000, 2))

     valueBox(tags$b(valor, style = "font-size: 400%;"),paste0("Milhões de MZN Desembolsados (",round(sum(disbursed$disbursed_usd)/42000000*100, 2),"%)"),icon = icon("landmark"), color = "info")
   })


   output$cumulativo_executado  <- renderValueBox({
     valor <- paste0(round(sum(full_approved_payments()$paid_ammount)/1000000, 2))

     valueBox(tags$b(valor, style = "font-size: 400%;"),paste0("Milhões de MZN Aplicados (",round(sum(full_approved_payments()$paid_ammount)/(42000000*64.46)*100, 2),"%)"),icon = icon("comments-dollar"), color = "success")
   })

   output$executado_rpsf  <- renderValueBox({
     valor_rpsf <- full_approved_payments() %>% dplyr::filter(pdr_financiers_en %in% c("RPSF 2nd Allocation", "RPSF 1st Allocation"))
     valor <- paste0(round(sum(valor_rpsf$paid_ammount)/1000000,2))

     valueBox(tags$b(valor, style = "font-size: 400%;"),paste0("Milhões de MZN do RPSF - COVID (",round(sum(valor_rpsf$paid_ammount)/(1698945*64.46)*100, 2),"%)"),icon = icon("coins"), color = "warning")
   })

   output$comparticipacao_governo  <- renderValueBox({
     valor_governo <-full_approved_payments() %>% dplyr::filter(pdr_financiers_en %in% c("Government"))
     valor <- paste0(round(sum(valor_governo$paid_ammount)/1000000,2))

     valueBox(tags$b(valor, style = "font-size: 400%;"),paste0("Milhões de MZN do Governo (",round(sum(valor_governo$paid_ammount)/(5453821*64.46)*100, 2),"%)"),icon = icon("flag"), color = "teal")
   })

    output$components_quarters <- DT::renderDataTable({

      components_pd <-  dbGetQuery(pool, "SELECT * FROM procava.procavacomponents_full")
      components_pdr <- components_pd %>% select(components_pt, PDR = total_cost)
      full_approved_payments <-   full_approved_payments()
      costabs_summaries <- read_feather("pdr_costabs.feather")
      budgeted <- paao_granulado()
      pdr <- components_pd %>% select(componentnum_pt, components_pt, PDR = total_cost)

      components_pdr <- costabs_summaries %>% filter(class %in% c("components", "Total")) %>% select(ordering, components_pt=description_pt, component_nums, PDR = total_cost)
      budgeted <- budgeted %>% group_by(subcomponent) %>% summarize(AWPB = sum(granularusd))
      budgeted$subcomponent <- str_replace(budgeted$subcomponent, "omponent", "omponente")
      full_approved_payments$paid_ammount <-  full_approved_payments$paid_ammount/e_rate
      full_approved_payments$quarters <- quarters(full_approved_payments$submission_date)
      full_approved_payments$years <- year(full_approved_payments$submission_date)
      totais <- full_approved_payments %>% group_by(quarters) %>% summarize(montante = sum(paid_ammount))%>%
        pivot_wider(names_from = "quarters", values_from = "montante", values_fill = 0)
      completions <- full_approved_payments %>% group_by(components_pt, quarters) %>% summarize(montante = sum(paid_ammount)) %>%
        pivot_wider(names_from = "quarters", values_from = "montante", values_fill = 0) %>% adorn_totals("col")
      completions <- merge(completions, budgeted, by.x = "components_pt",by.y = "subcomponent", all = TRUE)
      completions <- merge(completions, pdr, by = "components_pt", all = TRUE)
      completions[is.na(completions)] <- 0
      C1 <- completions %>% dplyr::filter(componentnum_pt == "Componente 1") %>% adorn_totals("row", name = "Component 1") %>% arrange(components_pt)
      C2 <- completions %>% dplyr::filter(componentnum_pt == "Componente 2") %>% adorn_totals("row", name = "Component 2")%>% arrange(components_pt)
      C3 <- completions %>% dplyr::filter(componentnum_pt == "Componente 3") %>% adorn_totals("row", name = "Component 3")%>% arrange(components_pt)
      C4 <- completions %>% dplyr::filter(componentnum_pt == "Componente 4") %>% adorn_totals("row", name = "Component 4")%>% arrange(components_pt)
      completions <- dplyr::bind_rows(C1, C2, C3, C4) %>% dplyr::filter(!is.na(PDR))
      TT <- completions %>% filter(componentnum_pt=="-")
      TT <- add_row(TT, summarize(TT, across(where(is.numeric), ~ sum(., na.rm = T)))) %>% dplyr::filter(is.na(components_pt))
      TT$components_pt[is.na(TT$components_pt)] <- "Total"
      completions <- dplyr::bind_rows(completions, TT) %>% mutate(awpb_pct = Total/AWPB*100, pdr_pct = Total/PDR*100) %>% dplyr::select(-componentnum_pt)
      completion <-completions  %>% relocate(awpb_pct, .after = AWPB)

      datatable(completion, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>%
      formatCurrency(2:ncol(completions), '')
    })

   output$category_quarters <- DT::renderDataTable({

     budgeted <- paao_granulado()
     costabs_summaries <- read_feather("pdr_costabs.feather")
     full_approved_payments <-  full_approved_payments()
     full_approved_payments$quarters <- quarters(full_approved_payments$submission_date)
     full_approved_payments$years <- year(full_approved_payments$submission_date)
     categories_quarters <- full_approved_payments %>% group_by(pdr_category, quarters) %>% summarise(sum(paid_ammount)/e_rate) %>% spread(quarters, -pdr_category) %>% adorn_totals("col")
     pdr_categories <- costabs_summaries %>% dplyr::filter(class == "categories") %>% dplyr::select(component_nums, Description, PDR = total_cost)
     budget_categories <- budgeted %>% group_by(pdr_category) %>% summarise(AWPB = sum(granularusd))
     pdr_awpb <- merge(categories_quarters, budget_categories, by.x = "pdr_category", by.y = "pdr_category", all = TRUE)
     pdr_awpb_pdr <- merge(pdr_awpb, pdr_categories, by.x = "pdr_category", by.y = "Description", all = TRUE)
     pdr_awpb_pdr[is.na(pdr_awpb_pdr)] <- 0
     pdr_awpb_pdr <- pdr_awpb_pdr %>% adorn_totals("row")
     pdr_awpb_pdr <- pdr_awpb_pdr %>% mutate(AWPB_pct = Total/AWPB*100,
                                             PDR_pct = Total/PDR*100) %>% select(-pdr_category) %>% select(component_nums, everything())
     pdr_awpb_pdr$component_nums[pdr_awpb_pdr$component_nums == "-"] <- "Total"
     pdr_awpb_pdr <-pdr_awpb_pdr  %>% relocate(AWPB_pct, .after = AWPB) %>% relocate(PDR_pct, .after = PDR)

     datatable(pdr_awpb_pdr, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>%
       formatCurrency(2:ncol(pdr_awpb_pdr), '')

   })

   output$financiers_quarters <- DT::renderDataTable({

     budgeted <- paao_granulado()
     costabs_summaries <- read_feather("pdr_costabs.feather")
     full_approved_payments <- full_approved_payments()
     full_approved_payments$quarters <- quarters(full_approved_payments$submission_date)
     full_approved_payments$years <- year(full_approved_payments$submission_date)
     categories_quarters <- full_approved_payments %>% group_by(pdr_financiers_en, quarters) %>% summarise(sum(paid_ammount)/e_rate) %>% spread(quarters, -pdr_financiers_en) %>% adorn_totals("col")
     pdr_categories <- costabs_summaries %>% dplyr::filter(class == "financiers") %>% dplyr::select(component_nums, Description, PDR = total_cost)
     budget_categories <- budgeted %>% group_by(financiers) %>% summarise(AWPB = sum(granularusd))
     pdr_awpb <- merge(categories_quarters, budget_categories, by.x = "pdr_financiers_en", by.y = "financiers", all = TRUE)
     pdr_awpb_pdr <- merge(pdr_awpb, pdr_categories, by.x = "pdr_financiers_en", by.y = "Description", all = TRUE)
     pdr_awpb_pdr[is.na(pdr_awpb_pdr)] <- 0
     pdr_awpb_pdr <- pdr_awpb_pdr %>% adorn_totals("row")
     pdr_awpb_pdr <- pdr_awpb_pdr %>% mutate(AWPB_pct = Total/AWPB*100,
                                             PDR_pct = Total/PDR*100) %>% select(-pdr_financiers_en) %>% select(component_nums, everything())
     pdr_awpb_pdr$component_nums[pdr_awpb_pdr$component_nums == "-"] <- "Total"
     pdr_awpb_pdr <-pdr_awpb_pdr  %>% relocate(AWPB_pct, .after = AWPB) %>% relocate(PDR_pct, .after = PDR)

     datatable(pdr_awpb_pdr, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>%
       formatCurrency(2:ncol(pdr_awpb_pdr), '')
   })

   output$execucao_ced <- DT::renderDataTable({

     budgeted <- paao_granulado()
     full_approved_payments <-  full_approved_payments()
     e_sistafe <-  dbGetQuery(pool, "SELECT ced, e_sistafe_pt FROM fiduciary.esistafe_ced")
     full_approved_payments$ced <- ifelse(is.na(full_approved_payments$ced), parse_number(full_approved_payments$e_sistafe_w_code), full_approved_payments$ced)
     full_approved_payments$quarters <- quarters(full_approved_payments$submission_date)
     full_approved_payments$years <- year(full_approved_payments$submission_date)
     categories_quarters <- full_approved_payments %>% group_by(ced, quarters) %>%
       summarise(montante = sum(paid_ammount)/e_rate) %>%
       pivot_wider(names_from = "quarters", values_from = "montante", values_fill = 0) %>% adorn_totals("col")
     budget_categories <- budgeted %>% group_by(ced) %>% summarise(AWPB = sum(granularusd))
     pdr_awpb <- merge(categories_quarters, budget_categories, by = "ced", all=TRUE)
     pdr_awpb <- merge(pdr_awpb, e_sistafe, by = "ced")
     pdr_awpb[is.na(pdr_awpb)] <- 0
     pdr_awpb <- pdr_awpb %>% adorn_totals("row")
     pdr_awpb <- pdr_awpb %>% mutate(AWPB_pct = Total/AWPB*100) %>% select(ced, e_sistafe_pt, everything())
     datatable(pdr_awpb, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>%
       formatCurrency(3:ncol(pdr_awpb), '')
   })
   
   output$informacao_proposta <- DT::renderDataTable({
     req(input$info_proposta)
     inFile <- input$info_proposta
     ficheiro <- read_excel(inFile$datapath, sheet = "DATASET")
     ficheiro$paid_ammount <-  as.numeric(ficheiro$paid_ammount)
     payments_dataset <- ficheiro %>% dplyr::filter(!is.na(paid_ammount))
     ficheiro_visivel <- payments_dataset
     ficheiro_visivel <- ficheiro_visivel %>% dplyr::select(beneficiary, awpb_id, processo_numero, detailed, payment_details, paid_ammount, contract,
                                                            e_sistafe_w_code, funding, applicant_name, everything())
     datatable(ficheiro_visivel, options=list(columnDefs = list(list(visible=FALSE, targets=c(13:ncol(ficheiro_visivel)))), rowsGroup = list(2)))
     
   })
   
   observeEvent(input$submeter_informacao_proposta, priority = 20, {
     req(input$info_proposta)
     inFile <- input$info_proposta
     ficheiro <- read_excel(inFile$datapath, sheet = "DATASET", col_types = c('text', 'text', 'numeric', 'text', 'text', 'text', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'text', 'text', 'text', 'text', 'date', 'date', 'numeric', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'numeric', 'text', 'text', 'text', 'text', 'text', 'numeric', 'numeric', 'text', 'date', 'date', 'text', 'numeric', 'numeric', 'numeric', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'numeric', 'text', 'text', 'text', 'text', 'date', 'text', 'text'))
     payments_dataset <- ficheiro %>% dplyr::filter(!is.na(paid_ammount))
     payments_dataset$ced <- parse_number(payments_dataset$e_sistafe_w_code)
     payments_dataset <- payments_dataset %>% dplyr::select(description, beneficiary, nuit, document, awpb_id, payment_details, units, quantity, price, paid_ammount,
                                                            contract, e_sistafe_w_code, detailed, relevance, started, ended, tompro_pay, ben_district, cost_centre,
                                                            process_type, process_status, funding, product_location, payment_order, submission_date, payment_date, pendings, 
                                                            government_contrib, private_contrib, third_party, processo_numero, expense_request_uid, planning_approval, 
                                                            procurement_approval, finance_approval, payment_ticket, overall_approval, comments, currency, quantity10, 
                                                            approval_comments, staff_id, is_contract, urgencia_processo, document_type, payment_checklist, update_date,
                                                            quantity30, quantity100, justificativo_tipo, subject, ifad_money, benef_contrib, ced)
     dbWriteTable(pool, SQL("fiduciary.payment_requests"), payments_dataset, overwrite = FALSE, append = TRUE)
     showModal(modalDialog(title=paste0("Pedido submetido"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
   })

  ################ INICIO DO PROBLEMA  ####################

   iv<- InputValidator$new()
   iv$add_rule("nuit", sv_between(100000000, 599999999, message_fmt = "NUIT correcto deve estar entre {left} e {right}."))
   iv$add_rule("processo_numero", sv_between(0, 999, message_fmt = "Numerador deve estar entre {left} e {right}."))
   iv$add_rule("value", sv_between(0, 100000000, message_fmt = "Deve estar entre {left} e {right}."))
   iv$add_rule("contract_level", sv_between(0, 100, message_fmt = "Valor correcto deve estar entre {left} e {right}. Idealmente nunca deveria passar 100"))
   iv$add_rule("contract", sv_required(message = "É obrigatório indicar o contrato"))
   iv$enable()

   observeEvent(input$submit, {
     processo_payment <-  getSurveyData()

     processo_payment <-  processo_payment %>% dplyr::select(question_id, response)
     processo_payment$response[processo_payment$response== "HIDDEN-QUESTION"] <- NA

     processo_payment <- setNames(data.frame(t(processo_payment[-1])), processo_payment[,1])

     processo_payment <- processo_payment
     setnames(processo_payment, "rubricas", "e_sistafe_w_code")
     setnames(processo_payment, "status", "process_status")
     setnames(processo_payment, "document_type", "justificativo_tipo")
     processo_payment <- processo_payment %>% mutate(ced = readr::parse_number(as.character(e_sistafe_w_code))) %>% select(-contract_level)

     processo_payment$process_type <- "Pagamento de fundos cobrados por fornecedores"
     processo_payment$product_location <- processo_payment$ben_district
     processo_payment$subject <- processo_payment$description

     processo_payment <- processo_payment %>% separate(awpb_coding, c('awpb_id','tompro_pay'), sep = " -- ", extra='drop')
     processo_payment <- processo_payment %>% separate(contract, c('contract'), sep = " : ", extra='drop')
     processo_payment <- processo_payment %>% separate(applicant, c('applicant','staff_id'), sep = " -- ", extra='drop')

     processo_payment$planning_approval <- "PENDENTE"
     processo_payment$overall_approval <- "PENDENTE"
     processo_payment$procurement_approval <- "PENDENTE"
     processo_payment$finance_approval <- "PENDENTE"
     processo_payment$approval_comments <- "Duplo clique para comentar"
     processo_payment$is_contract <- "YES"
     processo_payment$quantity100 <- processo_payment$quantity

     processo_payment$payment_ticket <- "Duplo clique e digite a OP"

     processo_payment$expense_request_uid <- toupper(str_trim(paste0(processo_payment$document, "-", processo_payment$nuit, "-", year(as.Date(processo_payment$submission_date))), side="both"))
     dbWriteTable(pool, SQL("fiduciary.payment_requests"), value =  processo_payment, append = TRUE, overwrite = FALSE, row.names = FALSE)
     
     qry4 = paste0("UPDATE fiduciary.aggreements SET  physical_compl = ", ifelse(is.na(processo_payment$contract_level[1]),0,processo_payment$contract_level[1])," WHERE contract_number ='",processo_payment$contract,"'")
     # dbExecute(pool, qry4)
      showModal(modalDialog(title=paste0("Pedido submetido. Aguarde pelo despacho!"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
   })

   ##############################  fim dO PROBLEMA 
   observeEvent(input$escrever_ip, {
     payments_data <- proposta_de_pagamento()
     tipo_despesa <- "Salários e remunerações"
     
     file_name <- ifelse(input$despesa_a_pagar == "Ajudas de custos", "Modelo_IP_RMarkdown_Final_ADC.docx",
                         ifelse(input$despesa_a_pagar =="Salários e remunerações", "Modelo_IP_RMarkdown_Final_SALARIOS.docx",
                                ifelse(input$despesa_a_pagar == "Pagamento de contratos", "Modelo_IP_RMarkdown_Final_CONTRATOS.docx",
                                       "Modelo_IP_RMarkdown_Final_OUTROS.docx")))
     sample_doc <- read_docx(file_name)
     
     keywords <- ifelse(input$despesa_a_pagar == "Ajudas de custos", "acordo com a tabela a seguir.",
                        ifelse(input$despesa_a_pagar =="Salários e remunerações", "ANEXO. DETALHES DO PAGAMENTO",
                               "ANEXO. DETALHES DO PAGAMENTO"))
     # file_name
     date_proposal <- as.Date(input$data_formulacao, "%m/%d/%y")
     month_number <- month(date_proposal)
     year_proposal <- year(date_proposal)
     # year_proposal
     formated_date <- paste0(day(date_proposal),"/", ifelse(month(date_proposal)<10,paste0("0",month(date_proposal)), month(date_proposal)), "/", year(date_proposal))
     # formated_date
     month_salaries_paid <- ifelse(month_number== 1, "Janeiro", ifelse(month_number== 2, "Fevereiro", ifelse(month_number== 3, "Março", ifelse(month_number== 4, "Abril",
                                                                                                                                               ifelse(month_number== 5, "Maio", ifelse(month_number== 6, "Junho", ifelse(month_number== 7, "Julho", ifelse(month_number== 8, "Agosto",
                                                                                                                                                                                                                                                           ifelse(month_number== 9, "Setembro", ifelse(month_number== 10, "Outubro",
                                                                                                                                                                                                                                                                                                       ifelse(month_number== 11, "Novembro", ifelse(month_number== 12, "Dezembro"))))))))))))

     # payments_data <- payments_data %>% dplyr::filter(tipos_de_despesas == tipo_despesa)
     payments_data <- payments_data %>% mutate_if(is.numeric, ~replace_na(., 0))
     
     ################   SUMMARY FOR OTHERS
     payments_summary <- payments_data %>% group_by(ced, e_sistafe_pt, beneficiary, nuit) %>% summarize(qty = sum(quantity30)*0.3 + sum(quantity10)*0.1 + sum(quantity100),
                                                                                                        valor = sum(paid_ammount))
     payments_summary$qty <- ifelse(payments_summary$qty == 0, 1, payments_summary$qty)
     payments_summary$preco <- payments_summary$valor/payments_summary$qty
     
     payments_summary <-  modify_if(payments_summary, ~is.numeric(.), ~round(., 2))
     payments_summary$e_sistafe_pt[is.na(payments_summary$e_sistafe_pt)] <- ""
     payments_summary$nuit[is.na(payments_summary$nuit)] <- ""
     payments_summary[is.na(payments_summary)] <- 0
     payments_summary <- payments_summary %>% adorn_totals("row", fill = "", col_var_name = c("valor"))
     payments_summary <- payments_summary %>% select(ced, e_sistafe_pt, qty, preco, everything())
     payments_summary$ced <- as.character(payments_summary$ced)
     setnames(payments_summary, c("ced", "e_sistafe_pt", "beneficiary", "nuit", "qty", "preco", "valor"), c("CED", "Designação do CED", "Beneficiário", "NUIT", "Qty", "Preço" , "Valor (MT)"))
     
     ############ SUMMARY AJUDAS DE CUSTOS
     payments_summary2 <- payments_data %>% group_by(ced, e_sistafe_pt, beneficiary, nuit) %>% summarize(qty = sum(quantity30)*0.3 + sum(quantity10)*0.1 + sum(quantity100),
                                                                                                         valor = sum(paid_ammount))
     payments_summary2$qty <- ifelse(payments_summary2$qty == 0, 1, payments_summary2$qty)
     payments_summary2$preco <- payments_summary2$valor/payments_summary2$qty
     payments_summary2 <-  modify_if(payments_summary2, ~is.numeric(.), ~round(., 2))
     payments_summary2$e_sistafe_pt[is.na(payments_summary2$e_sistafe_pt)] <- ""
     payments_summary2$nuit[is.na(payments_summary2$nuit)] <- ""
     payments_summary2[is.na(payments_summary2)] <- 0
     payments_summary2 <- payments_summary2 %>% adorn_totals("row", fill = "", col_var_name = c("valor"))
     payments_summary2 <- payments_summary2 %>% select(ced, e_sistafe_pt, beneficiary, nuit,qty, preco, everything())
     payments_summary2$ced <- as.character(payments_summary2$ced) 
     setnames(payments_summary2, c("ced", "e_sistafe_pt", "beneficiary", "nuit", "qty", "preco", "valor"), c("CED", "Designação do CED", "Beneficiário", "NUIT", "Qty", "Preço" , "Valor (MT)"))
     
     # payments_summary <- ifelse(tipo_despesa == "Ajudas de custos", payments_summary2, payments_summary)
     set_flextable_defaults(font.size = 9, font.color = "black", table.layout = "fixed", digits = 1)
     ft <- flextable(payments_summary)
     ft <- theme_booktabs(ft)
     ft <- autofit(ft)
     ft <- colformat_double(x = ft, big.mark=",", digits = 2, na_str = "N/A")
     ft <- align_nottext_col(ft, align = "right")
     ft <- line_spacing(ft, space = 1.0, part = "all")
     ft <- bold(ft, bold = TRUE, part = "header")
     ft <- bold(ft, i = nrow(payments_summary), bold = TRUE)
     ft <- padding(ft, padding = 1)
     ft <- compose(ft, i = nrow(payments_summary), j = 3, as_paragraph(as_chunk('-')))
     ft <- compose(ft, i = nrow(payments_summary), j = 4, as_paragraph(as_chunk('-')))
     ft <- compose(ft, i = nrow(payments_summary), j = 5, as_paragraph(as_chunk('-')))
     
     set_flextable_defaults(font.size = 9, font.color = "black", table.layout = "fixed", digits = 1)
     ft2 <- flextable(payments_summary2)
     ft2 <- theme_booktabs(ft2)
     ft2 <- autofit(ft2)
     ft2 <- colformat_double(x = ft2, big.mark=",", digits = 2, na_str = "N/A")
     ft2 <- align_nottext_col(ft2, align = "right")
     ft2 <- line_spacing(ft2, space = 1.0, part = "all")
     ft2 <- bold(ft2, bold = TRUE, part = "header")
     ft2 <- bold(ft2, i = nrow(payments_summary), bold = TRUE)
     ft2 <- padding(ft2, padding = 1)
     ft2 <- compose(ft2, i = nrow(payments_summary), j = 3, as_paragraph(as_chunk('-')))
     ft2 <- compose(ft2, i = nrow(payments_summary), j = 4, as_paragraph(as_chunk('-')))
     ft2 <- compose(ft2, i = nrow(payments_summary), j = 5, as_paragraph(as_chunk('-')))
     ft2_ajudas <- compose(ft2, i = nrow(payments_summary), j = 6, as_paragraph(as_chunk('-')))
     # ft2_ajudas
     
     sample_doc <- cursor_reach(sample_doc, keyword = keywords)
     sample_doc <- body_add_flextable(sample_doc, value = ft, pos = "after")
     
     document <- glue_collapse(payments_data$document, ", ", last = " e ")
     financiadores <- payments_data %>% group_by(details_contributors) %>% summarize(sum(paid_ammount)) %>% adorn_totals("col")
     financiadores$percent <-  financiadores$Total/sum(payments_data$paid_ammount)*100
     financiadores$contribs <- paste0(financiadores$details_contributors, " (", round(financiadores$percent,2), "%)")
     funding <- glue_collapse(financiadores$contribs, ", ", last = " e ")
     
     sample_doc <- body_replace_all_text(sample_doc, old_value = "FUNDO DE FOMENTO AGRÁRIO E EXTENSÃO RURAL, FUNDO PÚBLICO (FAR, FP)", new_value = toupper(paste0(payments_data$instituicao[1])), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "MINISTÉRIO DA AGRICULTURA E DESENVOLVIMENTO RURAL", new_value = toupper(paste0(payments_data$entidade_governo[1])), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "UNIDADE_GESTORA", new_value = toupper(paste0(payments_data$unidade_gestora[1])), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "supplier_name", new_value = paste0(payments_data$beneficiary[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "submission_date", new_value = paste0(as.character(payments_data$submission_date[1], format = "%d/%m/%Y")), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "subcomponente_pt", new_value = paste0(payments_data$componentnames_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "subcompo_desc", new_value = paste0(payments_data$components_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "sectores", new_value = paste0(payments_data$sectores[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "relevance", new_value = paste0(payments_data$relevance[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "process_type", new_value = paste0(payments_data$process_type[1]), only_at_cursor = FALSE, fixed = TRUE)
     
     sample_doc <- body_replace_all_text(sample_doc, old_value = "date_proposal_printed", new_value = formated_date, only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "month_salaries_paid", new_value = month_salaries_paid, only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "year_proposal", new_value = paste0(year_proposal), only_at_cursor = FALSE, fixed = TRUE)
     
     sample_doc <- body_replace_all_text(sample_doc, old_value = "processo_numero.", new_value = paste0(payments_data$processo_numero[1], "/", payments_data$cost_centers[1], "/", payments_data$sectores[1], "/032.22/", year(date_proposal)), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "processos_numeros", new_value = paste0(payments_data$processo_numero[1], only_at_cursor = FALSE, fixed = TRUE))
     
     sample_doc <- body_replace_all_text(sample_doc, old_value = "physical_compl", new_value = paste0(payments_data$physical_compl[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "paid_ammount", new_value = paste0(format(round(sum(payments_data$paid_ammount), 2), big.mark=",", nsmall = 2, scientific=FALSE)), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "funding", new_value = funding, only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "new_salute", new_value = paste0(payments_data$new_salute[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "new_approver", new_value = paste0(payments_data$new_approver[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "name_applicant", new_value = paste0(payments_data$name[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "financial_compl", new_value = paste0(payments_data$financial_compl[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "e_sistafe_w_code", new_value = paste0(payments_data$e_sistafe_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "documents_numbers", new_value = document, only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "detailed", new_value = paste0(payments_data$detailed[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "awpb_description", new_value = paste0(payments_data$descricao_da_actividade[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "cost_centers", new_value = paste0(payments_data$cost_centers[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "contractdescription_pt", new_value = paste0(payments_data$contractdescription_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "contract", new_value = paste0(payments_data$contract_number[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "contract", new_value = paste0(payments_data$contract_number[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "categoria_pdr", new_value = paste0(payments_data$categoria_pdr[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "carreira_proponente", new_value = paste0(payments_data$categoria[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "awpb_id", new_value = paste0(payments_data$awpb_id[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- footers_replace_all_text(sample_doc, old_value = "encedereco_alvo", new_value = paste0(payments_data$address[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- footers_replace_all_text(sample_doc, old_value = "cidade_alvo", new_value = paste0(payments_data$city[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "role", new_value = paste0(payments_data$short_vaccancy[1]), only_at_cursor = FALSE, fixed = TRUE)
     print(sample_doc, target = file.path(Sys.getenv("Home"), paste0("IP_", payments_data$paao_code[1], "_", input$despesa_a_pagar, ".docx")))
     showModal(modalDialog(title=paste0("DOCUMENTO SALVO NO 'MY DOCUMENTS'!"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
   })

   output$downloadData <- downloadHandler(
     filename <- function(){
       payments_data <- proposta_de_pagamento()
       tipo_despesa <- "Salários e remunerações"
       file_name <- ifelse(input$despesa_a_pagar == "Ajudas de custos", "Modelo_IP_RMarkdown_Final_ADC.docx",
                           ifelse(input$despesa_a_pagar =="Salários e remunerações", "Modelo_IP_RMarkdown_Final_SALARIOS.docx",
                                  ifelse(input$despesa_a_pagar == "Pagamento de contratos", "Modelo_IP_RMarkdown_Final_CONTRATOS.docx",
                                         "Modelo_IP_RMarkdown_Final_OUTROS.docx")))
       sample_doc <- read_docx(file_name)
       
       keywords <- ifelse(input$despesa_a_pagar == "Ajudas de custos", "acordo com a tabela a seguir.",
                          ifelse(input$despesa_a_pagar =="Salários e remunerações", "ANEXO. DETALHES DO PAGAMENTO",
                                 "ANEXO. DETALHES DO PAGAMENTO"))
       
       date_proposal <- as.Date(input$data_formulacao, "%m/%d/%y")
       month_number <- month(date_proposal)
       year_proposal <- year(date_proposal)
       formated_date <- paste0(day(date_proposal),"/", ifelse(month(date_proposal)<10,paste0("0",month(date_proposal)), month(date_proposal)), "/", year(date_proposal))
       month_salaries_paid <- ifelse(month_number== 1, "Janeiro", ifelse(month_number== 2, "Fevereiro", ifelse(month_number== 3, "Março", ifelse(month_number== 4, "Abril",
                                                                                                                                                 ifelse(month_number== 5, "Maio", ifelse(month_number== 6, "Junho", ifelse(month_number== 7, "Julho", ifelse(month_number== 8, "Agosto",
                                                                                                                                                                                                                                                             ifelse(month_number== 9, "Setembro", ifelse(month_number== 10, "Outubro",
                                                                                                                                                                                                                                                                                                         ifelse(month_number== 11, "Novembro", ifelse(month_number== 12, "Dezembro"))))))))))))

       payments_data <- payments_data %>% mutate_if(is.numeric, ~replace_na(., 0))
       
       ################   SUMMARY FOR OTHERS
       payments_summary <- payments_data %>% group_by(ced, e_sistafe_pt, beneficiary, nuit) %>% summarize(qty = sum(quantity30)*0.3 + sum(quantity10)*0.1 + sum(quantity100),
                                                                                                          valor = sum(paid_ammount))
       payments_summary$qty <- ifelse(payments_summary$qty == 0, 1, payments_summary$qty)
       payments_summary$preco <- payments_summary$valor/payments_summary$qty
       
       payments_summary <-  modify_if(payments_summary, ~is.numeric(.), ~round(., 2))
       payments_summary$e_sistafe_pt[is.na(payments_summary$e_sistafe_pt)] <- ""
       payments_summary$nuit[is.na(payments_summary$nuit)] <- ""
       payments_summary[is.na(payments_summary)] <- 0
       payments_summary <- payments_summary %>% adorn_totals("row", fill = "", col_var_name = c("valor"))
       payments_summary <- payments_summary %>% select(ced, e_sistafe_pt, qty, preco, everything())
       payments_summary$ced <- as.character(payments_summary$ced)
       setnames(payments_summary, c("ced", "e_sistafe_pt", "beneficiary", "nuit", "qty", "preco", "valor"), c("CED", "Designação do CED", "Beneficiário", "NUIT", "Qty", "Preço" , "Valor (MT)"))
       
       ############ SUMMARY AJUDAS DE CUSTOS
       payments_summary2 <- payments_data %>% group_by(ced, e_sistafe_pt, beneficiary, nuit) %>% summarize(qty = sum(quantity30)*0.3 + sum(quantity10)*0.1 + sum(quantity100),
                                                                                                           valor = sum(paid_ammount))
       payments_summary2$qty <- ifelse(payments_summary2$qty == 0, 1, payments_summary2$qty)
       payments_summary2$preco <- payments_summary2$valor/payments_summary2$qty
       payments_summary2 <-  modify_if(payments_summary2, ~is.numeric(.), ~round(., 2))
       payments_summary2$e_sistafe_pt[is.na(payments_summary2$e_sistafe_pt)] <- ""
       payments_summary2$nuit[is.na(payments_summary2$nuit)] <- ""
       payments_summary2[is.na(payments_summary2)] <- 0
       payments_summary2 <- payments_summary2 %>% adorn_totals("row", fill = "", col_var_name = c("valor"))
       payments_summary2 <- payments_summary2 %>% select(ced, e_sistafe_pt, beneficiary, nuit,qty, preco, everything())
       payments_summary2$ced <- as.character(payments_summary2$ced) 
       setnames(payments_summary2, c("ced", "e_sistafe_pt", "beneficiary", "nuit", "qty", "preco", "valor"), c("CED", "Designação do CED", "Beneficiário", "NUIT", "Qty", "Preço" , "Valor (MT)"))
       
       # payments_summary <- ifelse(tipo_despesa == "Ajudas de custos", payments_summary2, payments_summary)
       
       set_flextable_defaults(font.size = 9, font.color = "black", table.layout = "fixed", digits = 1)
       ft <- flextable(payments_summary)
       ft <- theme_booktabs(ft)
       ft <- autofit(ft)
       ft <- colformat_double(x = ft, big.mark=",", digits = 2, na_str = "N/A")
       ft <- align_nottext_col(ft, align = "right")
       ft <- line_spacing(ft, space = 1.0, part = "all")
       ft <- bold(ft, bold = TRUE, part = "header")
       ft <- bold(ft, i = nrow(payments_summary), bold = TRUE)
       ft <- padding(ft, padding = 1)
       ft <- compose(ft, i = nrow(payments_summary), j = 3, as_paragraph(as_chunk('-')))
       ft <- compose(ft, i = nrow(payments_summary), j = 4, as_paragraph(as_chunk('-')))
       ft <- compose(ft, i = nrow(payments_summary), j = 5, as_paragraph(as_chunk('-')))
       # ft
       
       set_flextable_defaults(font.size = 9, font.color = "black", table.layout = "fixed", digits = 1)
       ft2 <- flextable(payments_summary2)
       ft2 <- theme_booktabs(ft2)
       ft2 <- autofit(ft2)
       ft2 <- colformat_double(x = ft2, big.mark=",", digits = 2, na_str = "N/A")
       ft2 <- align_nottext_col(ft2, align = "right")
       ft2 <- line_spacing(ft2, space = 1.0, part = "all")
       ft2 <- bold(ft2, bold = TRUE, part = "header")
       ft2 <- bold(ft2, i = nrow(payments_summary), bold = TRUE)
       ft2 <- padding(ft2, padding = 1)
       ft2 <- compose(ft2, i = nrow(payments_summary), j = 3, as_paragraph(as_chunk('-')))
       ft2 <- compose(ft2, i = nrow(payments_summary), j = 4, as_paragraph(as_chunk('-')))
       ft2 <- compose(ft2, i = nrow(payments_summary), j = 5, as_paragraph(as_chunk('-')))
       ft2_ajudas <- compose(ft2, i = nrow(payments_summary), j = 6, as_paragraph(as_chunk('-')))
       # ft2_ajudas
       
       sample_doc <- cursor_reach(sample_doc, keyword = keywords)
       sample_doc <- body_add_flextable(sample_doc, value = ft, pos = "after")
       
       document <- glue_collapse(payments_data$document, ", ", last = " e ")
       financiadores <- payments_data %>% group_by(details_contributors) %>% summarize(sum(paid_ammount)) %>% adorn_totals("col")
       financiadores$percent <-  financiadores$Total/sum(payments_data$paid_ammount)*100
       financiadores$contribs <- paste0(financiadores$details_contributors, " (", round(financiadores$percent,2), "%)")
       funding <- glue_collapse(financiadores$contribs, ", ", last = " e ")
       
       sample_doc <- body_replace_all_text(sample_doc, old_value = "FUNDO DE FOMENTO AGRÁRIO E EXTENSÃO RURAL, FUNDO PÚBLICO (FAR, FP)", new_value = toupper(paste0(payments_data$instituicao[1])), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "MINISTÉRIO DA AGRICULTURA E DESENVOLVIMENTO RURAL", new_value = toupper(paste0(payments_data$entidade_governo[1])), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "UNIDADE_GESTORA", new_value = toupper(paste0(payments_data$unidade_gestora[1])), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "supplier_name", new_value = paste0(payments_data$beneficiary[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "submission_date", new_value = paste0(as.character(payments_data$submission_date[1], format = "%d/%m/%Y")), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "subcomponente_pt", new_value = paste0(payments_data$componentnames_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "subcompo_desc", new_value = paste0(payments_data$components_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "sectores", new_value = paste0(payments_data$sectores[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "relevance", new_value = paste0(payments_data$relevance[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "process_type", new_value = paste0(payments_data$process_type[1]), only_at_cursor = FALSE, fixed = TRUE)
       
       sample_doc <- body_replace_all_text(sample_doc, old_value = "date_proposal_printed", new_value = formated_date, only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "month_salaries_paid", new_value = month_salaries_paid, only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "year_proposal", new_value = paste0(year_proposal), only_at_cursor = FALSE, fixed = TRUE)
       
       sample_doc <- body_replace_all_text(sample_doc, old_value = "processo_numero.", new_value = paste0(payments_data$processo_numero[1]), only_at_cursor = FALSE, fixed = TRUE)
       
       sample_doc <- body_replace_all_text(sample_doc, old_value = "physical_compl", new_value = paste0(payments_data$physical_compl[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "paid_ammount", new_value = paste0(format(round(sum(payments_data$paid_ammount), 2), big.mark=",", nsmall = 2, scientific=FALSE)), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "funding", new_value = funding, only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "new_salute", new_value = paste0(payments_data$new_salute[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "new_approver", new_value = paste0(payments_data$new_approver[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "name_applicant", new_value = paste0(payments_data$name[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "financial_compl", new_value = paste0(payments_data$financial_compl[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "e_sistafe_w_code", new_value = paste0(payments_data$e_sistafe_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "documents_numbers", new_value = document, only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "detailed", new_value = paste0(payments_data$detailed[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "awpb_description", new_value = paste0(payments_data$descricao_da_actividade[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "cost_centers", new_value = paste0(payments_data$cost_centers[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "contractdescription_pt", new_value = paste0(payments_data$contractdescription_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "CONTRACT_NUMBER", new_value = paste0(payments_data$contract_number[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "contract", new_value = paste0(payments_data$contract_number[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "categoria_pdr", new_value = paste0(payments_data$categoria_pdr[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "carreira_proponente", new_value = paste0(payments_data$categoria[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "awpb_id", new_value = paste0(payments_data$awpb_id[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- footers_replace_all_text(sample_doc, old_value = "encedereco_alvo", new_value = paste0(payments_data$address[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- footers_replace_all_text(sample_doc, old_value = "cidade_alvo", new_value = paste0(payments_data$city[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_doc <- body_replace_all_text(sample_doc, old_value = "role", new_value = paste0(payments_data$short_vaccancy[1]), only_at_cursor = FALSE, fixed = TRUE)
       sample_docx <- print(sample_doc, target = file.path(Sys.getenv("Home"), paste0("IP_", payments_data$paao_code[1], "_", input$despesa_a_pagar, ".docx")))
       
       return (sample_docx)
     },
     content <- function(file) {file.copy(filename(), file)},
     
     contentType = "application/docx"
   )

   linhas_contratos <- reactive({input$linhas_contratos})
   colunas_contratos <- reactive({input$colunas_contratos})
   numerica_contratos<- reactive({input$numerica_contratos})
   output$contract_categories <- DT::renderDataTable({

     dataset_selecionado <- long_aggreements()
     sumario <- dataset_selecionado %>% group_by(awpb_role, contract_status) %>% summarise(valor_calculado = sum(revised_ammount))
     sumario <- sumario %>% pivot_wider(values_from = "valor_calculado", names_from = contract_status)
     sumario[is.na(sumario)] <- 0
     sumario <- sumario %>% adorn_totals("col") %>% adorn_totals("row")
     
     datatable(sumario, extensions = 'Buttons', options = list(dom = "Blfrtip",
                                                               buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download")),
                                                               lengthMenu = list( c(10, 20, -1), c(10, 20, "All")), pageLength = 10
     )) %>% formatCurrency(2:ncol(sumario), '')
   })

   output$component_contracts <- DT::renderDataTable({

     ##################  CONTRACTS COMPONENTS #######################
     totais <- long_aggreements() %>% group_by(contract_status) %>% summarize(montante = sum(revised_ammount))%>%
       pivot_wider(names_from = "contract_status", values_from = "montante", values_fill = 0)
     tt_pct <- totais
     tt_pct <- tt_pct[,1:ncol(tt_pct)]/sum(long_aggreements()$revised_ammount)*100
     tt_pct$components_pt <- "(%)"
     contract_components <- long_aggreements() %>% group_by(components_pt, componentnum_pt, contract_status) %>% summarize(montante = sum(revised_ammount)) %>%
       pivot_wider(names_from = "contract_status", values_from = "montante", values_fill = 0)
     # subcom_awpb <- read_feather("granular_awpb_2022.feather", columns = c("subcomp", "granularusd"))
     C1 <- contract_components %>% dplyr::filter(componentnum_pt == "Componente 1") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 1")) %>% arrange(components_pt)
     C2 <- contract_components %>% dplyr::filter(componentnum_pt == "Componente 2") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 2"))%>% arrange(components_pt)
     C3 <- contract_components %>% dplyr::filter(componentnum_pt == "Componente 3") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 3"))%>% arrange(components_pt)
     C4 <- contract_components %>% dplyr::filter(componentnum_pt == "Componente 4") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 4"))%>% arrange(components_pt)
     
     aggreement_components <- dplyr::bind_rows(C1, C2, C3, C4, totais, tt_pct)
     aggreement_components <- aggreement_components %>% dplyr::select(-2) %>% adorn_totals("col")
     aggreement_components$components_pt[is.na(aggreement_components$components_pt)] <- "Total"
     aggreement_components$col_share <- round(aggreement_components$Total/sum(long_aggreements()$revised_ammount)*100,2)
     
     datatable(aggreement_components, extensions = 'Buttons', options = list(dom = "Blfrtip",
                                                                             buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download")),
                                                                             lengthMenu = list( c(10, 20, -1), c(10, 20, "All")), pageLength = 10
     )) %>%
       formatCurrency(2:ncol(aggreement_components), '')
   })

   output$ammount_contracted <- renderPlot({
     ballance <- long_aggreements() %>% group_by(contract_manager, contract_status) %>% summarize(sum(revised_ammount)/1000)
     ballance$contract_status[ballance$contract_status == "Under implementation"] <- "Implementation"
     ballance$contract_manager[ballance$contract_manager == ""] <- "Unknown"
     ballance <- ballance %>% spread(contract_status, -contract_manager) %>% adorn_totals("col")
     ballance[is.na(ballance)] <- 0
     ballance$resolved <- ballance$Closed + ballance$Completed
     ballance <- ballance %>% arrange(-Total)
     ballance$contract_manager <- fct_reorder(ballance$contract_manager, ballance$Total, min)
     ballance <- ballance %>% mutate(place = if_else(row_number() == 1, 1.5, -1))
     ggplot(ballance, aes(x= contract_manager, y = Total)) + geom_bar(stat= "identity", col = "#c4fb83",fill="#c4fb83") +
       geom_bar(aes(x= contract_manager, y = resolved), stat= "identity", width = 0.5,col = "#10b005",fill="#10b005") + theme_void() +
       labs(x = "", y = "US$ nos contratos", caption = "Fonte: Actualizações dos Contratos @ FAR,FP - PROCAVA")+
       theme(axis.text.y = element_text(size = 10, hjust = 1), plot.margin = margin(rep(15, 4)))+
       geom_text(aes(label=round(Total), hjust = place), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
       coord_flip()

   })

   output$signed_contracts <- renderPlot({
     ballance <- long_aggreements() %>% group_by(contract_manager, contract_status) %>% summarize(n_distinct(contract_number))
     ballance$contract_status[ballance$contract_status == "Under implementation"] <- "Implementation"
     ballance$contract_manager[ballance$contract_manager == ""] <- "Unknown"
     ballance <- ballance %>% spread(contract_status, -contract_manager) %>% adorn_totals("col")
     ballance[is.na(ballance)] <- 0
     ballance$resolved <- ballance$Closed + ballance$Completed
     ballance <- ballance %>% arrange(-Total)
     ballance$contract_manager <- fct_reorder(ballance$contract_manager, ballance$Total, min)
     ballance <- ballance %>% mutate(place = if_else(row_number() == 1, 1.5, -1))
     ggplot(ballance, aes(x= contract_manager, y = Total)) + geom_bar(stat= "identity", col = "#c4fb83",fill="#c4fb83") +
       geom_bar(aes(x= contract_manager, y = resolved), stat= "identity", width = 0.5,col = "#10b005",fill="#10b005") + theme_void() +
       labs(x = "", y = "# de Contratos", caption = "Fonte: Actualizações dos Contratos @ FAR,FP - PROCAVA")+
       theme(axis.text.y = element_text(size = 10, hjust = 1), plot.margin = margin(rep(15, 4)))+
       geom_text(aes(label=round(Total), hjust = place), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
       coord_flip()

   })

   ####################  DOSSIERS VALUE BOXES
   output$dossiers_iniciados  <- renderValueBox({
     dossiers_iniciados <- filtered_dossiers() %>% dplyr::filter(procurement_stage != "Não iniciado")
     total_dossiers <- n_distinct(filtered_dossiers()$activ_id)
     iniciados <- n_distinct(dossiers_iniciados$activ_id)
     valueBox(tags$b(iniciados, style = "font-size: 400%;"), paste0("Dossiers Iniciados (",round(iniciados/total_dossiers*100, 2),"%)"), ionicon(name ="walk"), color = "success")
   })
   
   output$dossiers_contratados  <- renderValueBox({
     dossiers_contratados <- filtered_dossiers() %>% dplyr::filter(procurement_stage %in% c("Contrato Assinado", "Contrato visado", "Contrato em Implementação", "Contrato encerrado"))
     total_dossiers <- n_distinct(filtered_dossiers()$activ_id)
     dossiers_contratados <- n_distinct(dossiers_contratados$activ_id)
     valueBox(tags$b(dossiers_contratados, style = "font-size: 400%;"), paste0("Finalizados - contrato assinado (",round(dossiers_contratados/total_dossiers*100, 2),"%)"), ionicon(name ="book"), color = "orange")
   })
   
   output$usd_em_procurement   <- renderValueBox({
     dossiers_iniciados <- filtered_dossiers() %>% dplyr::filter(procurement_stage != "Não iniciado")
     total_usd <- sum(filtered_dossiers()$basic_usd)/1000000
     usd_dossiers_iniciados <- round(sum(dossiers_iniciados$basic_usd)/1000000,2)
     valueBox(tags$b(usd_dossiers_iniciados, style = "font-size: 400%;"), paste0("Milhões USD em Dossiers Iniciados (",round(usd_dossiers_iniciados/total_usd*100, 2),"%)"), ionicon(name ="cart"), color = "lightblue")
   })
   
   output$usd_latentes   <- renderValueBox({
     dossiers_latentes <- filtered_dossiers() %>% dplyr::filter(procurement_stage == "Não iniciado")
     total_usd <- round(sum(filtered_dossiers()$basic_usd)/1000000,0)
     usd_latentes <- round(sum(dossiers_latentes$basic_usd)/1000000,2)
     valueBox(tags$b(usd_latentes, style = "font-size: 400%;"), paste0("Milhões USD em Dossiers Não Iniciados (",round(usd_latentes/total_usd*100, 2),"%)"), ionicon(name ="warning"), color = "danger")
   })

   #  #####################  CONTRACT VALUE BOXES
    output$contratos_celebrados   <- renderValueBox({
      contratos_assinados <- n_distinct(long_aggreements()$contract_number)
      valueBox(tags$b(contratos_assinados, style = "font-size: 400%;"), paste0("Contratos Celebrados"), icon=icon("file"), color = "warning")
    })

    output$usd_contratados   <- renderValueBox({
      contratos_assinados <- round(sum(long_aggreements()$revised_ammount)/1000000,2)
      valueBox(tags$b(contratos_assinados, style = "font-size: 400%;"), paste0("Milhões USD Comprometidos"), icon=icon("dollar-sign"), color = "orange")
    })

    output$usd_pagos   <- renderValueBox({
      valor_pago <- round(sum(long_aggreements()$value_paid)/1000000,2)
      valueBox(tags$b(valor_pago, style = "font-size: 400%;"), paste0("Milhões de USD Pagos (", round(valor_pago/(sum(long_aggreements()$revised_ammount)/1000000),2),"%)"), icon=icon("money"), color = "lightblue")
    })


    output$contratos_fechados  <- renderValueBox({

      completed_contracts <- long_aggreements() %>% filter(contract_status %in% c("Closed", "Completed"))
      completed <- n_distinct(completed_contracts$contract_number)
      n_distinct(long_aggreements()$contract_number)

      valueBox(tags$b(completed, style = "font-size: 400%;"), paste0("Contratos fechados (", round(completed/n_distinct(long_aggreements()$contract_number)*100,2),"%)"), icon=icon("vote-yea"), color = "success")
    })
    
    
    fieldsMandatory <- c("procurement_stage", "comments_update")
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      shinyjs::toggleState(id = "submeter_dossier", condition = mandatoryFilled)
    })
    
    entry_form_ppdossiers <- function(button_id){
      showModal(
        modalDialog(
          div(id=("entry_form_ppdossiers"),
              tags$head(tags$style(".modal-dialog{ width:650px}")),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              fluidPage(
                fluidRow(h4("ACTUALIZAR CONTRATAÇÕES"),
                         splitLayout(cellWidths = c("20%", "80%"), cellArgs = list(style = "vertical-align: top"),
                                     selectizeInput("idpp", "Ref.ª", multiple = FALSE, choices = as.character(unique(pp_dataset$idpp))),
                                     textInput("activity_description_pt", "Descrição da Actividade")),
                         
                         splitLayout(cellWidths = c("50%", "50%"), cellArgs = list(style = "vertical-align: top"),           
                                     selectizeInput("procurement_stage", labelMandatory("Ponto de Situação"), multiple = FALSE, choices = pp_stages_pt),
                                     selectizeInput("method_name_pt", "Método de contratação", multiple = FALSE, choices = c(pp_method_names_pt))),
                         
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                     selectizeInput("ifad_review", "Revisão", multiple = FALSE, choices = c("Revisão Prévia" = "Prior Review", "Revisão Posterior" =  "Post Review")),            
                                     selectizeInput("non_consulting", "Não consultoria?", multiple = FALSE, choices = c("", "SIM"="Yes", "NÃO"="No")),
                                     selectizeInput("shortlisting_pp", "Lista-curta", multiple = FALSE, choices = c("SIM"="Yes","NÃO"="No"))),
                         
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                     selectizeInput("qualification", "Qualificação", multiple = FALSE, choices = c("Pré-qualificação" = "Pre-Qual", "Pós-qualificação" =  "Post-Qual")),
                                     numericInput("envelopes", "Envelopes", 1, min = 1, max = 2),
                                     numericInput("contract_usd", "Valor da Adjudicação", 0, min = 0, max = 40000000)),
                         
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                     dateInput("eo_i_submission", "Submissão de TDR", format = "yyyy-mm-dd"),
                                     dateInput("no_eo_i", "Não objecção TDR", format = "yyyy-mm-dd"),
                                     dateInput("inv_r_eo_i", "Convite para MDI", format = "yyyy-mm-dd")),
                         
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                     dateInput("close_r_eo_i", "Recepção de MDIs", format = "yyyy-mm-dd"),
                                     dateInput("report_eo_i", "Relatório de MDI", format = "yyyy-mm-dd"),
                                     dateInput("no_eo_i_report", "NO Relatório de MDI", format = "yyyy-mm-dd")),
                         
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                     dateInput("rfp_rno", "Documentos de Concurso", format = "yyyy-mm-dd"),
                                     dateInput("rfp_no", "NO Documentos de Concurso", format = "yyyy-mm-dd"),
                                     dateInput("invitation", "Anúncio de Concurso", format = "yyyy-mm-dd")),
                         
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                     dateInput("closing", "Recepção de Propostas", format = "yyyy-mm-dd"),
                                     dateInput("rno_eva_r", "Relatório Técnico", format = "yyyy-mm-dd"),
                                     dateInput("no_eva_r", "NO Relatório Técnico", format = "yyyy-mm-dd")),
                         
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                     dateInput("full_eva_rno", "Relatório Combinado", format = "yyyy-mm-dd"),
                                     dateInput("full_eva_no", "NO Relatório Combinado", format = "yyyy-mm-dd"),
                                     dateInput("noita", "Notificação de Adjudicação", format = "yyyy-mm-dd")),
                         
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                     dateInput("contract_awards", "Adjudicação", format = "yyyy-mm-dd"),
                                     dateInput("negotiations", "Negociações", format = "yyyy-mm-dd"),
                                     dateInput("rno_contract", "Draft do Contrato", format = "yyyy-mm-dd")),
                         
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                     dateInput("no_contract", "NO Draft do Contrato", format = "yyyy-mm-dd"),
                                     dateInput("signature", "Assinatura", format = "yyyy-mm-dd"),
                                     dateInput("contract_completion", "Conclusão", format = "yyyy-mm-dd")),
                         
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                     textInput("contract_n", "Número n.º"),
                                     textInput("vendor_id", "Contratada"),
                                     selectizeInput("updated_by", "Actualizado por", multiple = FALSE, choices = c('Amâncio António Nhantumbo' = '106803609', 'Andércio Vitane' = '111175047', 'Augusto Oreste' = '118185331', 'Luísa Ângela Josselina Calima' = '128504222', 'Daniel Lourenço Chitupila' = '103847915', 'Dionísia Castelo Machuza Cuna' = '123200871', 'Eduardo Marcos Cuamba' = '107958584', 'Ana Crimilda Fernando Silva' = '111730814', 'Baptista Ruben Ngine Zunguze' = '103149061', 'Ernesto Abrantes Dulamo Wane' = '104922521', 'Daniel Ozias Mate' = '103659124', 'Egídio Artur Alfredo Mutimba' = '103260116', 'Esperança David Muchanga' = '109197580', 'Eugénio Nhone' = '102542762', 'Gil Estevão Nhantumbo' = '100894343', 'Jerónimo Joaquim Francisco' = '110589123', 'Joaquim Daniel Macaringue' = '300249248', 'José Sancho Cumbi' = '100870630', 'Júlio Aguiar Bila' = '106864233', 'Júlio Marcelino Macaco' = '103160987', 'Lucas Albino Chiau' = '119429897', 'Lucília Santos' = '100864665', 'Manuel Tinga Mangueze' = '100868113', 'Neide Custódio Daniel' = '102032330', 'Neila Lúcia da Conceição Manjate' = '110757891', 'Rachida Jafar Abdul' = '115695673', 'Ibraimo Assuade Assane' = '104468241', 'Tiago Tiago' = '112841822', 'Joaquim Daniel Macaringue' = '108596120', 'Ámina Amade Mussá Faquirá' = '101202429', 'Nilza Racide Abdul Adolfo' = '118151771', 'Osvaldo Lázaro Banze' = '122094545', 'Rosário Aide' = '103846501'))),
                         
                         textAreaInput("comments_update", "Comentários", placeholder = "", height = 100, width = "100%"),
                         
                         helpText(labelMandatory(""), paste("Campo Obrigatório!")),
                         actionButton("submeter_dossier", "SALVAR", class = "btn-success")),
                
                footer = modalButton("Fechar"),
                easyClose = TRUE
              )
          )
        )
      )
    }
    
    fieldsAll <- c('activ_id', 'idpp', 'activity', 'is_grant', 'non_consultancies', 'activity_description_pt', 'shortlisting_pp', 
                   'ifad_review', 'qualification', 'proc_methods', 'envelopes', 'basic_usd', 'eo_i_submission', 'no_eo_i', 'inv_r_eo_i', 
                   'close_r_eo_i', 'report_eo_i', 'no_eo_i_report', 'rfp_rno', 'rfp_no', 'invitation', 'closing', 'rno_eva_r', 'no_eva_r', 
                   'full_eva_rno', 'full_eva_no', 'noita', 'contract_awards', 'negotiations', 'rno_contract', 'no_contract', 'signature', 
                   'contract_n', 'vendor_id', 'contract_usd', 'contract_completion', 'comments_update', 'lot', 'responsible', 'ifadpp_sheet', 
                   'method_name_pt', 'non_consulting', 'activity_pt', 'lotes', 'procurement_stage')
    
    observeEvent(input$edit_dossier, priority = 20,{
      SQL_ppdossiers <- filtered_dossiers()
      showModal(
        if(length(input$ppdossiers_rows_selected) > 1 ){
          modalDialog(
            title = "Warning",
            paste("Por favor, selecione só uma linha." ),easyClose = TRUE)
        } else if(length(input$ppdossiers_rows_selected) < 1){
          modalDialog(
            title = "Warning",
            paste("Por favor, selecione só uma linha." ),easyClose = TRUE)
        })  
      
      if(length(input$ppdossiers_rows_selected) == 1 ){
        entry_form_ppdossiers("submit_dossier")
        
        updateTextInput(session, "activity_description_pt", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "activity_description_pt"])
        updateSelectizeInput(session, "procurement_stage", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "procurement_stage"])
        updateSelectizeInput(session, "method_name_pt", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "method_name_pt"])
        updateSelectizeInput(session, "ifad_review", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "ifad_review"])
        updateSelectizeInput(session, "non_consulting", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "non_consulting"])
        updateSelectizeInput(session, "shortlisting_pp", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "shortlisting_pp"])
        updateSelectizeInput(session, "qualification", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "qualification"])
        updateSelectizeInput(session, "updated_by", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "updated_by"])
        
        updateSelectizeInput(session, "idpp", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "idpp"])
        
        updateNumericInput(session, "envelopes", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "envelopes"])
        updateNumericInput(session, "basic_usd", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "basic_usd"])
        updateNumericInput(session, "contract_usd", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "contract_usd"])
        updateNumericInput(session, "lot", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "lot"])
        
        updateDateInput(session, "no_eo_i", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "no_eo_i"])
        updateDateInput(session, "eo_i_submission", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "eo_i_submission"])
        updateDateInput(session, "no_eo_i", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "no_eo_i"])
        updateDateInput(session, "inv_r_eo_i", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "inv_r_eo_i"])
        updateDateInput(session, "close_r_eo_i", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "close_r_eo_i"])
        updateDateInput(session, "report_eo_i", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "report_eo_i"])
        updateDateInput(session, "no_eo_i_report", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "no_eo_i_report"])
        updateDateInput(session, "rfp_rno", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "rfp_rno"])
        updateDateInput(session, "rfp_no", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "rfp_no"])
        updateDateInput(session, "invitation", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "invitation"])
        updateDateInput(session, "closing", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "closing"])
        updateDateInput(session, "rno_eva_r", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "rno_eva_r"])
        updateDateInput(session, "no_eva_r", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "no_eva_r"])
        updateDateInput(session, "full_eva_rno", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "full_eva_rno"])
        updateDateInput(session, "full_eva_no", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "full_eva_no"])
        updateDateInput(session, "noita", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "noita"])
        updateDateInput(session, "contract_awards", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "contract_awards"])
        updateDateInput(session, "negotiations", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "negotiations"])
        updateDateInput(session, "rno_contract", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "rno_contract"])
        updateDateInput(session, "no_contract", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "no_contract"])
        updateDateInput(session, "signature", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "signature"])
        updateDateInput(session, "contract_completion", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "contract_completion"])
        
        updateTextAreaInput(session, "comments_update", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "comments_update"])
      }
    })
    
    output$ppdossiers <- DT::renderDataTable({
      contratacoes <-   filtered_dossiers()
      names(contratacoes) <- c('PAAO',  'PP',  'Actividade',  'Método',  'Orçamento (US$)',  'Responsável',  'Situação',  'Comentários',  'eo_i_submission',  'no_eo_i',  'inv_r_eo_i',  'close_r_eo_i',  'report_eo_i',  'no_eo_i_report',  'rfp_rno',  'rfp_no',  'invitation',  'closing',  'rno_eva_r',  'no_eva_r',  'full_eva_rno',  'full_eva_no',  'noita',  'contract_awards',  'negotiations',  'rno_contract',  'no_contract',  'signature',  'envelopes',  'contract_n',  'vendor_id',  'contract_usd',  'contract_completion',  'lot',  'ifadpp_sheet',  'ifad_review',  'qualification',  'proc_methods',  'non_consulting',  'activity_pt',  'lotes',  'is_grant',  'shortlisting_pp',  'non_consultancies',  'activity', 'simplexstage_en', 'components_pt','componentnum_pt')
      datatable(contratacoes, rownames= FALSE, options = list(columnDefs = list(list(targets = c(8:45), visible = FALSE)))) %>% formatCurrency('Orçamento (US$)', '') %>% 
        formatStyle('Situação', backgroundColor = styleEqual(pp_stages, pp_stages_colours))
    })
    
    observeEvent(input$submeter_dossier, priority = 1, {
      SQL_ppdossiers <- filtered_dossiers()
      row_selection <- SQL_ppdossiers[input$ppdossiers_rows_selected, "activ_id"]
      
      qry = paste0("UPDATE fiduciary.procurement_dossiers SET  comments_update = '", paste(input$comments_update),"', ",
                   "last_updated =  '", paste(as.character(format(Sys.Date()))),"', ",
                   "procurement_stage = '", paste(input$procurement_stage),"', ",
                   "proc_methods = '", paste(input$proc_methods),"', ",
                   "shortlisting_pp =  '", paste(as.character(format(input$shortlisting_pp))),"', ",
                   "ifad_review =  '", paste(as.character(format(input$ifad_review))),"', ",
                   "qualification =  '", paste(as.character(format(input$qualification))),"', ",
                   "envelopes  =  ", input$envelopes,", ",
                   "eo_i_submission    =  '", lubridate::ymd(if_else(is.na(input$eo_i_submission), ymd(NA),input$eo_i_submission)),"', ",
                   "no_eo_i    =  '", lubridate::ymd(if_else(is.na(input$no_eo_i), ymd(NA),input$no_eo_i)),"', ",
                   "inv_r_eo_i   =  '", lubridate::ymd(if_else(is.na(input$inv_r_eo_i ), ymd(NA),input$inv_r_eo_i )),"', ",
                   "close_r_eo_i  =  '", lubridate::ymd(if_else(is.na(input$close_r_eo_i), ymd(NA),input$close_r_eo_i)),"', ",
                   "report_eo_i   =  '", lubridate::ymd(if_else(is.na(input$report_eo_i ), ymd(NA),input$report_eo_i )),"', ",
                   "no_eo_i_report   =  '", lubridate::ymd(if_else(is.na(input$no_eo_i_report ), ymd(NA),input$no_eo_i_report )),"', ",
                   "rfp_rno   =  '", lubridate::ymd(if_else(is.na(input$rfp_rno ), ymd(NA),input$rfp_rno )),"', ",
                   "rfp_no  =  '", lubridate::ymd(if_else(is.na(input$rfp_no), ymd(NA),input$rfp_no)),"', ",
                   "invitation   =  '", lubridate::ymd(if_else(is.na(input$invitation ), ymd(NA),input$invitation )),"', ",
                   "closing   =  '", lubridate::ymd(if_else(is.na(input$closing ), ymd(NA),input$closing )),"', ",
                   "rno_eva_r   =  '", lubridate::ymd(if_else(is.na(input$rno_eva_r ), ymd(NA),input$rno_eva_r )),"', ",
                   "no_eva_r   =  '", lubridate::ymd(if_else(is.na(input$no_eva_r ), ymd(NA),input$no_eva_r )),"', ",
                   "full_eva_rno  =  '", lubridate::ymd(if_else(is.na(input$full_eva_rno), ymd(NA),input$full_eva_rno)),"', ",
                   "full_eva_no  =  '", lubridate::ymd(if_else(is.na(input$full_eva_no), ymd(NA),input$full_eva_no)),"', ",
                   "noita   =  '", lubridate::ymd(if_else(is.na(input$noita ), ymd(NA),input$noita )),"', ",
                   "non_consulting =  '", paste(as.character(format(input$non_consulting))),"', ",
                   "updated_by = '", paste(input$updated_by),"'",
                   " WHERE idpp = '", paste(input$idpp),"'")
      
      dbExecute(pool, qry)
      removeModal()
      showModal(modalDialog(title=paste0("Parabéns. Dossier actualizado!"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;", footer = modalButton("Fechar")))))
    })

    observeEvent(input$submeter_dossier, priority = 1, {
      SQL_ppdossiers <- filtered_dossiers()
      row_selection <- SQL_ppdossiers[input$ppdossiers_rows_selected, "activ_id"]
      
      qry = paste0("UPDATE fiduciary.procurement_dossiers SET  comments_update = '", paste(input$comments_update),"', ",
                   "contract_usd = ", as.numeric(ifelse(is.na(input$contract_usd),0,input$contract_usd)),", ",
                   
                   "contract_awards   =  '", lubridate::ymd(if_else(is.na(input$contract_awards ), ymd(NA),input$contract_awards )),"', ",
                   "negotiations   =  '", lubridate::ymd(if_else(is.na(input$negotiations ), ymd(NA),input$negotiations )),"', ",
                   "rno_contract   =  '", lubridate::ymd(if_else(is.na(input$rno_contract ), ymd(NA),input$rno_contract )),"', ",
                   "no_contract   =  '", lubridate::ymd(if_else(is.na(input$no_contract ), ymd(NA),input$no_contract )),"', ",
                   "signature   =  '", lubridate::ymd(if_else(is.na(input$signature ), ymd(NA),input$signature )),"', ",
                   "contract_n  =  '", paste(as.character(format(input$contract_n ))),"', ",
                   "vendor_id  =  '", paste(as.character(format(input$vendor_id))),"', ",
                   "contract_completion  =  '", lubridate::ymd(if_else(is.na(input$no_eo_i), ymd(NA),input$no_eo_i)),"' ",
                   "WHERE lotes = '", paste(input$lotes),"'")
      
      dbExecute(pool, qry)
      removeModal()
      showModal(modalDialog(title=paste0("Parabéns. Dossier actualizado!"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
    })

    fieldsMandatory <- c("contract_status", "comments")
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      shinyjs::toggleState(id = "submeter_aggreement", condition = mandatoryFilled)
    })
    
    entry_form_contractdossiers <- function(button_id){
      showModal(
        modalDialog(
          div(id=("entry_form_contractdossiers"),
              tags$head(tags$style(".modal-dialog{ width:800px}")),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              fluidPage(
                fluidRow(h3("ACTUALIZAR CONTRATAÇÕES"),
                         
                         splitLayout(cellWidths = c("100%"), cellArgs = list(style = "vertical-align: top"),
                                     h4(HTML("<b>DADOS DE BÁSICOS</b>"))),
                         
                         splitLayout(cellWidths = c("100%"), cellArgs = list(style = "vertical-align: top"),
                                     textInput("contractdescription_en", "Objecto (Inglês)")),
                         br(),
                         hr(style="border-color: purple"),
                         h4(HTML("<b>NUMERAÇÃO DO CONTRATO</b>")),
                         splitLayout(cellWidths = c("50%", "50%"), cellArgs = list(style = "vertical-align: top"),
                                     textInput("no_number", "Não objecção n.º"),
                                     selectizeInput("contract_number", "Contrato n.º", multiple = FALSE, choices = c("",unique(aggreements$contract_number)))),
                         
                         h4(HTML("<b>ENDEREÇO DA CONTRATADA</b>")),
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"),
                                     textInput("supplier_address", "Endereço"),
                                     selectizeInput("supplier_country", "País", multiple = FALSE, choices = c("", countries)),
                                     textInput("supplier_city", "Cidade")),
                         
                         splitLayout(cellWidths = c("50%", "50%"), cellArgs = list(style = "vertical-align: top"),
                                     textInput("supplier_email", labelMandatory("Email")),
                                     textInput("supplier_phone", labelMandatory("Telefone"))),
                         
                         h4(HTML("<b>GESTOR DO CONTRATO</b>")),
                         splitLayout(cellWidths = c("100%"), cellArgs = list(style = "vertical-align: top"),
                                     selectizeInput("contract_manager", labelMandatory("Nome do Gestor do Contrato"), multiple = FALSE, choices =c("", c('Amâncio António Nhantumbo' = '106803609', 'Andércio Vitane' = '111175047', 'Augusto Oreste' = '118185331', 'Luísa Ângela Josselina Calima' = '128504222', 'Daniel Lourenço Chitupila' = '103847915', 'Dionísia Castelo Machuza Cuna' = '123200871', 'Eduardo Marcos Cuamba' = '107958584', 'Ana Crimilda Fernando Silva' = '111730814', 'Baptista Ruben Ngine Zunguze' = '103149061', 'Ernesto Abrantes Dulamo Wane' = '104922521', 'Daniel Ozias Mate' = '103659124', 'Egídio Artur Alfredo Mutimba' = '103260116', 'Esperança David Muchanga' = '109197580', 'Eugénio Nhone' = '102542762', 'Gil Estevão Nhantumbo' = '100894343', 'Jerónimo Joaquim Francisco' = '110589123', 'Joaquim Daniel Macaringue' = '300249248', 'José Sancho Cumbi' = '100870630', 'Júlio Aguiar Bila' = '106864233', 'Júlio Marcelino Macaco' = '103160987', 'Lucas Albino Chiau' = '119429897', 'Lucília Santos' = '100864665', 'Manuel Tinga Mangueze' = '100868113', 'Neide Custódio Daniel' = '102032330', 'Neila Lúcia da Conceição Manjate' = '110757891', 'Rachida Jafar Abdul' = '115695673', 'Ibraimo Assuade Assane' = '104468241', 'Tiago Tiago' = '112841822', 'Joaquim Daniel Macaringue' = '108596120', 'Ámina Amade Mussá Faquirá' = '101202429', 'Nilza Racide Abdul Adolfo' = '118151771', 'Osvaldo Lázaro Banze' = '122094545', 'Rosário Aide' = '103846501')))),
                         
                         h4(HTML("<b>DATAS DO CONTRATO</b>")),
                         splitLayout(cellWidths = c("33.33333%", "33.33333%", "33.33333%", "33.33333%"), cellArgs = list(style = "vertical-align: top"),
                                     dateInput("sign_date", "Assinatura", format = "yyyy-mm-dd"),
                                     dateInput("start_date", "Início", format = "yyyy-mm-dd"),
                                     dateInput("end_date", "Fim", format = "yyyy-mm-dd")),
                         
                         h4(HTML("<b>PREÇO DO CONTRATO</b>")),
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"),
                                     selectizeInput("currency", labelMandatory("Moeda"), multiple = FALSE, choices = c("",moeda)),
                                     numericInput("ammount", "Montante inicial", 0, min = 0, max = 100000000),
                                     numericInput("exchange_rate", "Câmbio para USD", 64.46, min = 0, max = 10000)),
                         
                         h4(HTML("<b>ADENDA AO CONTRATO</b>")),
                         splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"),
                                     dateInput("revision_date", "Data de revisão", format = "yyyy-mm-dd"),
                                     numericInput("revised_ammount", "Montante revisto", 0, min = 0, max = 100000000),
                                     dateInput("revised_end", "Prazo revisto", format = "yyyy-mm-dd")),
                         
                         h4(HTML("<b>FONTES DE FINANCIAMENTO</b>")),
                         splitLayout(cellWidths = c("25%", "25%", "25%", "25%"), cellArgs = list(style = "vertical-align: top"),
                                     numericInput("ifadgrant_percent", "Donativo FIDA (%)", 0, min = 0, max = 100),
                                     numericInput("ifadloan_percent", "Crédito FIDA (%)", 0, min = 0, max = 100),
                                     numericInput("rpsf1_percent", "RPSF 1 (%)", 0, min = 0, max = 100),
                                     numericInput("rpsf2_percent", "RPSF 2 (%)", 0, min = 0, max = 100)),
                         
                         splitLayout(cellWidths = c("25%", "25%", "25%", "25%"), cellArgs = list(style = "vertical-align: top"),
                                     numericInput("government_percent", "Governo (%)", 0, min = 0, max = 100),
                                     numericInput("gcf_percent", "GCF (%)", 0, min = 0, max = 100),
                                     numericInput("beneficiary_percent", "Beneficiários (%)", 0, min = 0, max = 100),
                                     numericInput("others_percent", "Outros (%)", 0, min = 0, max = 100)),
                         
                         h4(HTML("<b>DESEMPENHO DO CONTRATO</b>")),
                         splitLayout(cellWidths = c("30%", "22.5%", "22.5%", "25%"), cellArgs = list(style = "vertical-align: top"),
                                     selectizeInput("contract_status", labelMandatory("Situação"), multiple = FALSE, choices = c("",contract_status)),
                                     numericInput("contract_perform", labelMandatory("Desempenho"), 0, min = 0, max = 4),
                                     numericInput("physical_compl", labelMandatory("Execução física"), 0, min = 0, max = 100),
                                     selectizeInput("risk_flag", labelMandatory("Risco"), multiple = FALSE, choices = c("",risk_level))),
                         
                         h4(HTML("<b>GARANTIA DEFINITIVA</b>")),
                         splitLayout(cellWidths = c("50%", "25%", "25%"), cellArgs = list(style = "vertical-align: top"),
                                     textInput("perfguar_numb", "Garantia n.º"),
                                     dateInput("perfguar_issuedate", "Emissão", format = "yyyy-mm-dd"),
                                     dateInput("perfguar_expdate", "Validade", format = "yyyy-mm-dd")),
                         
                         splitLayout(cellWidths = c("60%", "25%", "15%"), cellArgs = list(style = "vertical-align: top"),
                                     selectizeInput("perfguar_issuer", "Emissor", multiple = FALSE, choices = c("",guarantee_issuers)),
                                     selectizeInput("perfguar_type", "Tipo", multiple = FALSE, choices = c("",garantias)),
                                     numericInput("perfguar_value", "Valor", 0, min = 0, max = 6000000)),
                         
                         h4(HTML("<b>ADIANTAMENTOS AO CONTRATO</b>")),
                         splitLayout(cellWidths = c("25%", "30%", "45%"), cellArgs = list(style = "vertical-align: top"),
                                     numericInput("ammount_advanced", "Valor adiantado", 0, min = 0, max = 60000000),
                                     dateInput("advance_date", "Data do adiantamento", format = "yyyy-mm-dd"),
                                     textInput("advguar_numb", "Garantia n.º")),
                         
                         splitLayout(cellWidths = c("60%", "20%", "20%"), cellArgs = list(style = "vertical-align: top"),
                                     selectizeInput("advguar_issuer", "Emissor", multiple = FALSE, choices = c("",guarantee_issuers)),
                                     dateInput("advguar_issuedate", "Emissão", format = "yyyy-mm-dd"),
                                     dateInput("advguar_expdate", "Validade", format = "yyyy-mm-dd")),
                         
                         splitLayout(cellWidths = c("70%", "30%"), cellArgs = list(style = "vertical-align: top"),
                                     selectizeInput("advguar_type", "Tipo", multiple = FALSE, choices = c("",garantias)),
                                     numericInput("advguar_value", "Valor", 0, min = 0, max = 60000000)),
                         # dateInput("update_date", "Actualização", format = "yyyy-mm-dd"),
                         textAreaInput("comments", labelMandatory("COMENTÁRIOS"), placeholder = "", height = 100, width = "100%"),
                         
                         helpText(labelMandatory(""), paste("Campo Obrigatório!")),
                         actionButton("submeter_aggreement", "SALVAR", class = "btn-success")),
                easyClose = TRUE
              )
          )
        )
      )
    }
    
    output$contractdossiers <- DT::renderDataTable({
      contrato <-   filtered_aggreements()
      names(contrato) <- c('Contratada', 'Contraton.º', 'Assinatura', 'Prazo revisto', 'Objecto', 'Moeda', 'Montante revisto', 'Situação', 'Execução física (%)', 'Comentários', 'contractdescription_en', 'no_number', 'supplier_address', 'supplier_country', 'supplier_city', 'supplier_email', 'supplier_phone', 'contract_manager', 'start_date', 'end_date', 'ammount', 'exchange_rate', 'revision_date', 'ifadgrant_percent', 'ifadloan_percent', 'rpsf1_percent', 'rpsf2_percent', 'government_percent', 'gcf_percent', 'beneficiary_percent', 'others_percent', 'performance_guarantee_number', 'performance_guarantee_issue_date', 'performance_guarantee_expire_date', 'performance_guarantee_issuer', 'performance_guarantee_type', 'performance_guarantee_value', 'advance_date', 'advance_guarantee_number', 'advance_guarantee_issuer', 'advance_guarantee_issuedate', 'advance_guarantee_expire_date', 'advance_guarantee_type', 'advance_guarantee_value', 'update_date', 'Desempenho', 'Risco', 'Valor adiantado')
      datatable(contrato,  rownames=F, extensions = c('Buttons'), options = list(dom = 'Bfrtip', buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print'),  pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
                                                                              columnDefs = list(list(targets = c(10:47), visible = FALSE))))%>%
        
        formatStyle('Situação', backgroundColor = styleEqual(c("Em implementação"), c("#ff5334"))) %>% 

        formatCurrency('Montante revisto', '') %>% 
        formatDate(c("Assinatura", "Prazo revisto"), "toLocaleDateString") %>% 
        formatStyle(c('Assinatura','Prazo revisto','Moeda','Situação','Execução física (%)'), `text-align` = 'center')
    })
    
    observeEvent(input$edit_aggreements, priority = 20,{
      SQL_aggreements <- aggreements
      showModal(
        if(length(input$contractdossiers_rows_selected) > 1 ){
          modalDialog(
            title = "ALERTA",
            paste("Por favor, selecione só uma linha." ),easyClose = TRUE)
        } else if(length(input$contractdossiers_rows_selected) < 1){
          modalDialog(
            title = "ALERTA",
            paste("Por favor, selecione só uma linha." ),easyClose = TRUE)
        })  
      
      if(length(input$contractdossiers_rows_selected) == 1 ){
        entry_form_contractdossiers("submit_aggreement")
        
        updateDateInput(session, "sign_date", value = SQL_aggreements[input$contractdossiers_rows_selected, "sign_date"])
        updateDateInput(session, "start_date", value = SQL_aggreements[input$contractdossiers_rows_selected, "start_date"])
        updateDateInput(session, "end_date", value = SQL_aggreements[input$contractdossiers_rows_selected, "end_date"])
        updateDateInput(session, "revision_date", value = SQL_aggreements[input$contractdossiers_rows_selected, "revision_date"])
        updateDateInput(session, "contract_number", value = SQL_aggreements[input$contractdossiers_rows_selected, "contract_number"])
        updateDateInput(session, "perfguar_issuedate", value = SQL_aggreements[input$contractdossiers_rows_selected, "perfguar_issuedate"])
        updateDateInput(session, "perfguar_expdate", value = SQL_aggreements[input$contractdossiers_rows_selected, "perfguar_expdate"])
        updateDateInput(session, "advance_date", value = SQL_aggreements[input$contractdossiers_rows_selected, "advance_date"])
        updateDateInput(session, "advguar_issuedate", value = SQL_aggreements[input$contractdossiers_rows_selected, "advguar_issuedate"])
        updateDateInput(session, "advguar_expdate", value = SQL_aggreements[input$contractdossiers_rows_selected, "advguar_expdate"])
        updateDateInput(session, "revised_end", value = SQL_aggreements[input$contractdossiers_rows_selected, "revised_end"])
        updateNumericInput(session, "revised_ammount", value = SQL_aggreements[input$contractdossiers_rows_selected, "revised_ammount"])
        updateNumericInput(session, "exchange_rate", value = SQL_aggreements[input$contractdossiers_rows_selected, "exchange_rate"])
        updateNumericInput(session, "ifadgrant_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "ifadgrant_percent"])
        updateNumericInput(session, "ifadloan_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "ifadloan_percent"])
        updateNumericInput(session, "currency", value = SQL_aggreements[input$contractdossiers_rows_selected, "currency"])
        updateNumericInput(session, "ammount", value = SQL_aggreements[input$contractdossiers_rows_selected, "ammount"])
        updateNumericInput(session, "rpsf1_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "rpsf1_percent"])
        updateNumericInput(session, "rpsf2_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "rpsf2_percent"])
        updateNumericInput(session, "government_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "government_percent"])
        updateNumericInput(session, "gcf_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "gcf_percent"])
        updateNumericInput(session, "beneficiary_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "beneficiary_percent"])
        updateNumericInput(session, "others_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "others_percent"])
        updateNumericInput(session, "contract_perform", value = SQL_aggreements[input$contractdossiers_rows_selected, "contract_perform"])
        updateNumericInput(session, "physical_compl", value = SQL_aggreements[input$contractdossiers_rows_selected, "physical_compl"])
        updateNumericInput(session, "perfguar_value", value = SQL_aggreements[input$contractdossiers_rows_selected, "perfguar_value"])
        updateNumericInput(session, "ammount_advanced", value = SQL_aggreements[input$contractdossiers_rows_selected, "ammount_advanced"])
        updateNumericInput(session, "advguar_value", value = SQL_aggreements[input$contractdossiers_rows_selected, "advguar_value"])
        updateSelectizeInput(session, "supplier_country", selected = SQL_aggreements[input$contractdossiers_rows_selected, "supplier_country"])
        updateSelectizeInput(session, "contract_manager", selected = SQL_aggreements[input$contractdossiers_rows_selected, "contract_manager"])
        updateSelectizeInput(session, "contract_status", selected = SQL_aggreements[input$contractdossiers_rows_selected, "contract_status"])
        updateSelectizeInput(session, "risk_flag", selected = SQL_aggreements[input$contractdossiers_rows_selected, "risk_flag"])
        updateSelectizeInput(session, "perfguar_issuer", selected = SQL_aggreements[input$contractdossiers_rows_selected, "perfguar_issuer"])
        updateSelectizeInput(session, "perfguar_type", selected = SQL_aggreements[input$contractdossiers_rows_selected, "perfguar_type"])
        updateSelectizeInput(session, "advguar_issuer", selected = SQL_aggreements[input$contractdossiers_rows_selected, "advguar_issuer"])
        updateSelectizeInput(session, "advguar_type", selected = SQL_aggreements[input$contractdossiers_rows_selected, "advguar_type"])
        updateTextInput(session, "supplier_address", value = SQL_aggreements[input$contractdossiers_rows_selected, "supplier_address"])
        updateTextInput(session, "supplier_city", value= SQL_aggreements[input$contractdossiers_rows_selected, "supplier_city"])
        updateTextInput(session, "supplier_email", value= SQL_aggreements[input$contractdossiers_rows_selected, "supplier_email"])
        updateTextInput(session, "supplier_phone", value= SQL_aggreements[input$contractdossiers_rows_selected, "supplier_phone"])
        updateTextInput(session, "no_number", value = SQL_aggreements[input$contractdossiers_rows_selected, "no_number"])
        updateTextInput(session, "perfguar_numb", value = SQL_aggreements[input$contractdossiers_rows_selected, "perfguar_numb"])
        updateTextInput(session, "advguar_numb", value = SQL_aggreements[input$contractdossiers_rows_selected, "advguar_numb"])
        updateTextInput(session, "supplier_name", value = SQL_aggreements[input$contractdossiers_rows_selected, "supplier_name"])
        updateTextInput(session, "contractdescription_en", value = SQL_aggreements[input$contractdossiers_rows_selected, "contractdescription_en"])
        updateTextAreaInput(session, "comments", value = SQL_aggreements[input$contractdossiers_rows_selected, "comments"])
      }
    })
    
    observeEvent(input$submeter_aggreement, priority = 1, {
      SQL_aggreements <- aggreements
      row_selection <- SQL_aggreements[input$contractdossiers_rows_selected, "contract_number"]
      
      qry = paste0("UPDATE fiduciary.aggreements SET  comments = '", paste(input$comments),"', ",
                   
                   "update_date =  '", paste(as.character(format(Sys.Date()))),"', ",
                   "contractdescription_en = '", paste(input$contractdescription_en),"', ",
                   "supplier_address = '", paste(input$supplier_address),"', ",
                   "supplier_country = '", paste(input$supplier_country),"', ",
                   "supplier_city = '", paste(input$supplier_city),"', ",
                   "supplier_email = '", paste(input$supplier_email),"', ",
                   "supplier_phone = '", paste(input$supplier_phone),"', ",
                   "contract_manager = '", paste(input$contract_manager),"', ",
                   "no_number = '", paste(input$no_number),"', ",
                   "sign_date = '", paste(input$sign_date),"', ",
                   "start_date = '", paste(input$start_date),"', ",
                   "end_date = '", paste(input$end_date),"', ",
                   "revised_ammount = '", paste(input$revised_ammount),"', ",
                   "exchange_rate = '", paste(input$exchange_rate),"', ",
                   "ifadgrant_percent = '", paste(input$ifadgrant_percent),"', ",
                   "ifadloan_percent = '", paste(input$ifadloan_percent),"', ",
                   "rpsf1_percent = '", paste(input$rpsf1_percent),"', ",
                   "rpsf2_percent = '", paste(input$rpsf2_percent),"', ",
                   "government_percent = '", paste(input$government_percent),"', ",
                   "gcf_percent = '", paste(input$gcf_percent),"', ",
                   "beneficiary_percent = '", paste(input$beneficiary_percent),"', ",
                   "others_percent = '", paste(input$others_percent),"', ",
                   "revision_date = '", paste(input$revision_date),"', ",
                   "contract_status = '", paste(input$contract_status),"', ",
                   "contract_perform = '", paste(input$contract_perform),"', ",
                   "physical_compl = '", paste(input$physical_compl),"', ",
                   "risk_flag = '", paste(input$risk_flag),"', ",
                   "perfguar_numb = '", paste(input$perfguar_numb),"', ",
                   "perfguar_issuedate = '", paste(input$perfguar_issuedate),"', ",
                   "perfguar_expdate = '", paste(input$perfguar_expdate),"', ",
                   "perfguar_issuer = '", paste(input$perfguar_issuer),"', ",
                   "perfguar_type = '", paste(input$perfguar_type),"', ",
                   "perfguar_value = '", paste(input$perfguar_value),"', ",
                   "ammount_advanced = '", paste(input$ammount_advanced),"', ",
                   "advance_date = '", paste(input$advance_date),"', ",
                   "advguar_numb = '", paste(input$advguar_numb),"', ",
                   "advguar_issuedate = '", paste(input$advguar_issuedate),"', ",
                   "advguar_expdate = '", paste(input$advguar_expdate),"', ",
                   "advguar_issuer = '", paste(input$advguar_issuer),"', ",
                   "advguar_type = '", paste(input$advguar_type),"', ",
                   "advguar_value = '", paste(input$advguar_value),"', ",
                   "revised_end = '", paste(input$revised_end),"' ",
                   
                   " WHERE contract_number = '", paste(input$contract_number),"'")
      
      dbExecute(pool, qry)
      removeModal()
      showModal(modalDialog(title=paste0("Parabéns. Contrato actualizado!"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
    })

    output$dados_processo <- DT::renderDataTable({
      table  <- proposta_de_pagamento() %>% adorn_totals("row", fill = "-", col_var_name = c("paid_ammount"))
      names(table) <- c('CED', 'Designação da CED', 'Beneficiário à creditar', 'NUIT', 'Documento', 'Quantidade', 'Montante (MZN)', 'processo_numero', 'awpb_id', 'description', 'tipo_despesas', 'submission_date', 'payment_date', 'units', 'name', 'detailed', 'quantity10', 'quantity30', 'quantity100', 'e_sistafe_w_code', 'tipos_de_despesas', 'contract', 'contract_number', 'justificativo_tipo', 'funding', 'contractdescription_pt', 'physical_compl', 'financial_compl', 'process_type', 'staff', 'relevance', 'tompro_pay', 'paao_code', 'costab_code', 'descricao_da_actividade', 'subcomp', 'tompro_key', 'categoria_pdr', 'pdr_category', 'componentnames_pt', 'componentes_pt', 'components_pt', 'componentnum_pt', 'cost_centers', 'sectores', 'short_vaccancy', 'categoria', 'ugb', 'new_salute', 'new_approver', 'unidade_gestora', 'address', 'city', 'instituicao', 'entidade_governo', 'pdr_financiers_en', 'pdr_financiers_pt', 'details_contributors')
      datatable(table,  rownames=F, extensions = c('Buttons'), options = list(dom = 'Bfrtip', 
                               buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print'),  pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
                               columnDefs = list(list(targets = c(7:(ncol(proposta_de_pagamento())-1)), visible = FALSE))))%>%
        formatCurrency(c('Quantidade', 'Montante (MZN)'), '')
    })

    output$situacao_pp_responsaveis <- renderPlot({
      responsible_status <- filtered_dossiers() %>% group_by(responsible, simplexstage_en) %>% summarize(sum(basic_usd)/1000) %>% spread(simplexstage_en, -responsible) %>% adorn_totals("col")
      responsible_status[is.na(responsible_status)] <- 0
      responsible_status$Iniciados <- responsible_status$Total - responsible_status$`Latente`
      responsible_status$responsible <- fct_reorder(responsible_status$responsible, responsible_status$Total, min)
      ggplot(responsible_status, aes(x= responsible, y = Total)) + geom_bar(stat= "identity", col = "#c4fb83",fill="#c4fb83") +
        geom_bar(aes(x= responsible, y = Iniciados), stat= "identity", width = 0.5,col = "#10b005",fill="#10b005") + theme_void() +
        labs(x = "", y = "US$ em licitação", caption = "Fonte: Actualizações do PP @ FAR,FP - PROCAVA")+
        theme(axis.text.y = element_text(size = 10))+
        geom_text(aes(label=round(Total), hjust = 1.5), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
        coord_flip()
    })

    output$dossiers_latentes_responsavel <- renderPlot({
      responsible_status <- filtered_dossiers() %>% group_by(responsible, simplexstage_en) %>% summarize(sum(basic_usd)/1000) %>% spread(simplexstage_en, -responsible) %>% adorn_totals("col")
      responsible_status[is.na(responsible_status)] <- 0
      responsible_status$responsible <- fct_reorder(responsible_status$responsible, responsible_status$Total, min)
      ggplot(responsible_status, aes(x= responsible, y = Total)) + geom_bar(stat= "identity", col = "#fea471",fill="#fea471") +
        geom_bar(aes(x= responsible, y = Latente), stat= "identity", width = 0.5,col = "#f73622", fill = "#f73622") + theme_void() +
        labs(x = "", y = "US$ em licitação", caption = "Fonte: Actualizações do PP @ FAR,FP - PROCAVA")+
        theme(axis.text.y = element_text(size = 10))+
        geom_text(aes(label=round(Total), hjust = 1.5), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
        coord_flip()
    })

    output$dossiers_methods <- DT::renderDataTable({

      categories_pp <- filtered_dossiers()  %>% group_by(proc_methods, ifadpp_sheet, simplexstage_en) %>% summarize(montante = n_distinct(activ_id)) %>% pivot_wider(names_from = simplexstage_en, values_from = montante)
      ctots  <-   filtered_dossiers()  %>% group_by(ifadpp_sheet, simplexstage_en) %>% summarize(montante = n_distinct(activ_id)) %>%
        pivot_wider(names_from = simplexstage_en, values_from = montante) %>% adorn_totals("row") %>% dplyr::filter(ifadpp_sheet== "Total")
      categories_pp  <- categories_pp  %>% dplyr::arrange(ifadpp_sheet) %>% split( .[,"ifadpp_sheet"] ) %>% purrr::map_df(., janitor::adorn_totals)
      categories_pp $proc_methods <- ifelse(categories_pp $ifadpp_sheet == "-", lag(categories_pp $ifadpp_sheet),  categories_pp $proc_methods)
      categories_pp $ifadpp_sheet <- ifelse(categories_pp $ifadpp_sheet == "-", categories_pp $proc_methods,  categories_pp $ifadpp_sheet)
      categories_pp$proc_methods[categories_pp$proc_methods == ""] <- "Outros"
      categories_pp  <- categories_pp  %>% arrange(ifadpp_sheet, proc_methods)
      ctots_pct <- ctots[, -1]/n_distinct(filtered_dossiers()$activ_id)*100
      categories_pp  <- dplyr::bind_rows(categories_pp , ctots, ctots_pct) %>% adorn_totals("col")
      categories_pp$proc_methods[is.na(categories_pp $proc_methods)] <- "Total"
      categories_pp_activities  <- categories_pp  %>% select(-ifadpp_sheet)

      setnames(categories_pp_activities, "proc_methods", "Categorias e Métodos")
      datatable(categories_pp_activities, rownames = FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>%
        formatCurrency(2:ncol(categories_pp_activities), '')
    })

    output$dossiers_components <- DT::renderDataTable({
      components_pp <- filtered_dossiers()  %>% group_by(components_pt, componentnum_pt, simplexstage_en) %>% summarize(montante = n_distinct(activ_id)) %>% pivot_wider(names_from = simplexstage_en, values_from = montante)
      ctots  <-   filtered_dossiers()  %>% group_by(componentnum_pt, simplexstage_en) %>% summarize(montante = n_distinct(activ_id)) %>%
        pivot_wider(names_from = simplexstage_en, values_from = montante) %>% adorn_totals("row") %>% dplyr::filter(componentnum_pt== "Total")
      components_pp <- components_pp %>% dplyr::arrange(componentnum_pt) %>% split( .[,"componentnum_pt"] ) %>% purrr::map_df(., janitor::adorn_totals)
      components_pp$components_pt <- ifelse(components_pp$componentnum_pt == "-", lag(components_pp$componentnum_pt),  components_pp$components_pt)
      components_pp$componentnum_pt <- ifelse(components_pp$componentnum_pt == "-", components_pp$components_pt,  components_pp$componentnum_pt)
      components_pp <- components_pp %>% arrange(componentnum_pt, components_pt)
      ctots_pct <- ctots[, -1]/n_distinct(filtered_dossiers()$activ_id)*100
      components_pp <- dplyr::bind_rows(components_pp, ctots, ctots_pct) %>% adorn_totals("col")
      components_pp$components_pt[is.na(components_pp$components_pt)] <- "Total"
      components_pp_activities <- components_pp %>% select(-componentnum_pt)
      setnames(components_pp_activities, "components_pt", "Componentes")
      datatable(components_pp, rownames = FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>%
        formatCurrency(2:ncol(components_pp), '')
    })

    output$dossiers_components_usd <- DT::renderDataTable({
      components_pp <- filtered_dossiers()  %>% group_by(components_pt, componentnum_pt, simplexstage_en) %>% summarize(montante = sum(basic_usd)) %>% pivot_wider(names_from = simplexstage_en, values_from = montante)
      ctots  <-   filtered_dossiers()  %>% group_by(componentnum_pt, simplexstage_en) %>% summarize(montante = sum(basic_usd)) %>%
        pivot_wider(names_from = simplexstage_en, values_from = montante) %>% adorn_totals("row") %>% dplyr::filter(componentnum_pt== "Total")
      components_pp <- components_pp %>% dplyr::arrange(componentnum_pt) %>% split( .[,"componentnum_pt"] ) %>% purrr::map_df(., janitor::adorn_totals)
      components_pp$components_pt <- ifelse(components_pp$componentnum_pt == "-", lag(components_pp$componentnum_pt),  components_pp$components_pt)
      components_pp$componentnum_pt <- ifelse(components_pp$componentnum_pt == "-", components_pp$components_pt,  components_pp$componentnum_pt)
      components_pp <- components_pp %>% arrange(componentnum_pt, components_pt)
      ctots_pct <- ctots[, -1]/sum(filtered_dossiers()$basic_usd)*100
      components_pp <- dplyr::bind_rows(components_pp, ctots, ctots_pct) %>% adorn_totals("col")
      components_pp$components_pt[is.na(components_pp$components_pt)] <- "Total"
      components_pp_value <- components_pp %>% select(-componentnum_pt)
      setnames(components_pp_value, "components_pt", "Componentes")
      datatable(components_pp_value, rownames = FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>%
        formatCurrency(2:ncol(components_pp_value), '')
    })

    output$dossiers_methods_usd <- DT::renderDataTable({
      categories_pp <- filtered_dossiers()  %>% group_by(proc_methods, ifadpp_sheet, simplexstage_en) %>% summarize(montante = sum(basic_usd)) %>% pivot_wider(names_from = simplexstage_en, values_from = montante)
      ctots  <-   filtered_dossiers()  %>% group_by(ifadpp_sheet, simplexstage_en) %>% summarize(montante = sum(basic_usd)) %>%
        pivot_wider(names_from = simplexstage_en, values_from = montante) %>% adorn_totals("row") %>% dplyr::filter(ifadpp_sheet== "Total")
      categories_pp  <- categories_pp  %>% dplyr::arrange(ifadpp_sheet) %>% split( .[,"ifadpp_sheet"] ) %>% purrr::map_df(., janitor::adorn_totals)
      categories_pp $proc_methods <- ifelse(categories_pp $ifadpp_sheet == "-", lag(categories_pp $ifadpp_sheet),  categories_pp $proc_methods)
      categories_pp $ifadpp_sheet <- ifelse(categories_pp $ifadpp_sheet == "-", categories_pp $proc_methods,  categories_pp $ifadpp_sheet)
      categories_pp$proc_methods[categories_pp$proc_methods == ""] <- "Outros"
      categories_pp  <- categories_pp  %>% arrange(ifadpp_sheet, proc_methods)
      ctots_pct <- ctots[, -1]/sum(filtered_dossiers()$basic_usd)*100
      categories_pp  <- dplyr::bind_rows(categories_pp , ctots, ctots_pct) %>% adorn_totals("col")
      categories_pp $proc_methods[is.na(categories_pp $proc_methods)] <- "Total"
      categories_pp_value  <- categories_pp  %>% select(-ifadpp_sheet)
      setnames(categories_pp_value, "proc_methods", "Categorias e Métodos")

      datatable(categories_pp_value, rownames = FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>%
        formatCurrency(2:ncol(categories_pp_value), '')
    })

    
    
    observeEvent(input$lastClick, priority = 20,{
      SQL_df <- filtered_awpb()
      
        updateTextAreaInput(session, "descricao_da_actividade", value = SQL_df[input$responses_table_rows_selected, "descricao_da_actividade"])
        updateSelectInput(session, "situacao", selected = SQL_df[input$responses_table_rows_selected, "situacao"])
        updateSelectInput(session, "awpb_id", selected = SQL_df[input$responses_table_rows_selected, "awpb_id"])
        updateTextAreaInput(session, "comentarios", value = SQL_df[input$responses_table_rows_selected, "comentarios"])
        # updateSelectInput(session, "awpb_ids", selected = SQL_df[input$responses_table_rows_selected, "awpb_id"])
    })
    
    
    
    
    
    observeEvent(input$submeter_actividade, priority = 10, {
      
      SQL_df <- filtered_awpb()
      row_selection <- SQL_df[input$responses_table_rows_selected, "awpb_id"]
      qry = paste0("UPDATE procava.awpb_updates SET  comentarios = '", paste(input$comentarios),"', ", 
                   "data_updated =  '", paste(as.character(format(Sys.Date(), format="%Y-%m-%d"))),"', ",
                   "situacao = '", paste(input$situacao),"' ",
                   " WHERE awpb_id = '", paste(row_selection),"'")
      
      dbGetQuery(pool, qry)
      showModal(modalDialog(title=paste0("Actividade Actualizada"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
    })

    
    
    observeEvent(input$recalcular_realizacoes, priority = 10, {

      
      location_achievements <- DBI::dbGetQuery(pool, "SELECT awpb_id, distrito_beneficiario, value_achieved, province_odk FROM procava.achieved_targets_regions WHERE value_achieved >0")
      location_achievements$value_achieved <- as.numeric(location_achievements$value_achieved)
      
      ###################  PAAO DISTRITAL
      paao_distrital <- location_achievements %>% group_by(awpb_id, distrito_beneficiario) %>% summarize(achieved = sum(value_achieved)) %>% pivot_wider(names_from = distrito_beneficiario, values_from = achieved)
      paao_distrital <- paao_distrital %>% adorn_totals("col", name = "achieved_national") %>% mutate_all(as.character) %>% select(awpb_id, achieved_national, everything())
      
      for(i in colnames(paao_distrital)) {
        for(j in 1:nrow(paao_distrital)) {
          paao_distrital[j,i] <-  ifelse(is.na(paao_distrital[j,i]), NA, paste0(i, " (", paao_distrital[j,i], ")"))
        }
      }
      
      paao_distrital <- paao_distrital  %>% unite("distrital_locations", 3:ncol(paao_distrital), sep = ", ", na.rm = TRUE, remove = FALSE)
      paao_distrital$achieved_national  <- as.numeric(gsub("[^[:digit:]]+", "", paao_distrital$achieved_national))
      paao_distrital$awpb_id <- gsub("[awpb_id (]", "", paao_distrital$awpb_id)
      paao_distrital$awpb_id <- gsub("[)]", "", paao_distrital$awpb_id)
      paao_distrital <- paao_distrital[, c("awpb_id", "distrital_locations", "achieved_national")]
      
      ###################  PAAO NIASSA
      paao_niassa <- location_achievements %>% dplyr::filter(province_odk %in% c("NIASSA", "UPGPN")) %>% group_by(awpb_id, distrito_beneficiario) %>% summarize(achieved = sum(value_achieved)) %>% pivot_wider(names_from = distrito_beneficiario, values_from = achieved)
      paao_niassa <- paao_niassa %>% adorn_totals("col", name = "achieved_niassa") %>% mutate_all(as.character) %>% select(awpb_id, achieved_niassa, everything())
      
      for(i in colnames(paao_niassa)) {
        for(j in 1:nrow(paao_niassa)) {
          paao_niassa[j,i] <-  ifelse(is.na(paao_niassa[j,i]), NA, paste0(i, " (", paao_niassa[j,i], ")"))
        }
      }
      
      paao_niassa <- paao_niassa  %>% unite("distrital_niassa", 3:ncol(paao_niassa), sep = ", ", na.rm = TRUE, remove = FALSE)
      paao_niassa$achieved_niassa  <- as.numeric(gsub("[^[:digit:]]+", "", paao_niassa$achieved_niassa))
      
      paao_niassa$awpb_id <- gsub("[awpb_id (]", "", paao_niassa$awpb_id)
      paao_niassa$awpb_id <- gsub("[)]", "", paao_niassa$awpb_id)
      paao_niassa <- paao_niassa[, c("awpb_id", "distrital_niassa", "achieved_niassa")]

      ###################  PAAO  ZONA SUL
      paao_sul <- location_achievements %>% dplyr::filter(province_odk %in% c("MAPUTO CIDADE", "URGPS", "GAZA", "MAPUTO", "INHAMBANE")) %>% group_by(awpb_id, distrito_beneficiario) %>% summarize(achieved = sum(value_achieved)) %>% pivot_wider(names_from = distrito_beneficiario, values_from = achieved)
      paao_sul <- paao_sul %>% adorn_totals("col", name = "achieved_sul") %>% mutate_all(as.character) %>% select(awpb_id, achieved_sul, everything())
      
      for(i in colnames(paao_sul)) {
        for(j in 1:nrow(paao_sul)) {
          paao_sul[j,i] <-  ifelse(is.na(paao_sul[j,i]), NA, paste0(i, " (", paao_sul[j,i], ")"))
        }
      }
      
      paao_sul <- paao_sul  %>% unite("distrital_sul", 3:ncol(paao_sul), sep = ", ", na.rm = TRUE, remove = FALSE)
      paao_sul$achieved_sul <- as.numeric(gsub("[^[:digit:]]+", "", paao_sul$achieved_sul))
      
      paao_sul$awpb_id <- gsub("[awpb_id (]", "", paao_sul$awpb_id)
      paao_sul$awpb_id <- gsub("[)]", "", paao_sul$awpb_id)
      paao_sul <- paao_sul[, c("awpb_id", "distrital_sul", "achieved_sul")]
      
      ###################  PROVINCIAL
      provincial <- location_achievements %>% group_by(awpb_id, province_odk) %>% summarize(achieved = sum(value_achieved)) %>% pivot_wider(names_from = province_odk, values_from = achieved)
      provincial <-provincial %>% mutate_all(as.character)
      
      for(i in colnames(provincial)) {
        for(j in 1:nrow(provincial)) {
          provincial[j,i] <-  ifelse(is.na(provincial[j,i]), NA, paste0(i, " (", provincial[j,i], ")"))
        }
      }
      
      provincial <- provincial  %>% unite("province_locations", 3:ncol(provincial), sep = ", ", na.rm = TRUE, remove = FALSE)
      provincial$awpb_id <- gsub("[awpb_id (]", "", provincial$awpb_id)
      provincial$awpb_id <- gsub("[)]", "", provincial$awpb_id)
      provincial <- provincial[, c("awpb_id", "province_locations")]
      paao_distrital <- merge(provincial, paao_distrital, by = "awpb_id")
      paao_distrital <- merge(paao_niassa, paao_distrital, by = "awpb_id")
      paao_distrital <- merge(paao_sul, paao_distrital, by = "awpb_id")
      
      paao_distrital$awpb_id <- gsub(" ", "", paao_distrital$awpb_id)
      
      S1 <- c('CABO_DELGADO', 'GAZA', 'INHAMBANE', 'MANICA', 'MAPUTO', 'MAPUTO_CIDADE', 'NAMPULA', 'NIASSA', 'SOFALA', 'TETE', 'ZAMBEZIA', 'ESTRANGEIRO', 'Ancuabe', 'Balama', 'Chiure', 'Ibo', 'Macomia', 'Mecufi', 'Meluco', 'Metuge', 'Mocimboa_da_Praia', 'Montepuez', 'Mueda', 'Namuno', 'Nangade', 'Palma', 'Pemba', 'Quissanga', 'Estrangeiro', 'Bilene', 'Chibuto', 'Chicualacuala', 'Chigubo', 'Chokwe', 'Chongoene', 'Guija', 'Limpopo', 'Mabalane', 'Mandlakazi', 'Mapai', 'Massangena', 'Massingir', 'Xai_Xai', 'Funhalouro', 'Govuro', 'Homoine', 'Inhambane', 'Inharrime', 'Inhassoro', 'Jangamo', 'Mabote', 'Massinga', 'Maxixe', 'Morrumbene', 'Panda', 'Vilankulo', 'Zavala', 'Barue', 'Chimoio', 'Gondola', 'Guro', 'Macate', 'Machaze', 'Macossa', 'Manica', 'Mossurize', 'Sussundenga', 'Tambara', 'Vanduzi', 'Boane', 'Magude', 'Manhica', 'Marracuene', 'Matola', 'Matutuine', 'Moamba', 'Namaacha', 'Kamavota', 'KaMaxakeni', 'KaMphumu', 'KaMubukwana', 'KaNyaka', 'Katembe', 'Nlhamankulu', 'Angoche', 'Erati', 'Ilha_de_Mocambique', 'Lalaua', 'Larde', 'Liupo', 'Malema', 'Meconta', 'Mecuburi', 'Memba', 'Mogincual', 'Mogovolas', 'Moma', 'Monapo', 'Morrupula', 'Mossuril', 'Muecate', 'Nacala_a_Velha', 'Nacaroa', 'Nampula', 'Rapale', 'Ribaue', 'Chimbonila', 'Cuamba', 'Lago', 'Lichinga', 'Majune', 'Mandimba', 'Marrupa', 'Maua', 'Mavago', 'Mecanhelas', 'Mecula', 'Metarica', 'Muembe', 'Ngauma', 'Nipepe', 'Sanga', 'Beira', 'Buzi', 'Caia', 'Chemba', 'Cheringoma', 'Chibabava', 'Dondo', 'Gorongoza', 'Machanga', 'Maringue', 'Marromeu', 'Muanza', 'Nhamatanda', 'Angonia', 'Cahora_Bassa', 'Changara', 'Chifunde', 'Chiuta', 'Doa', 'Macanga', 'Magoe', 'Marara', 'Maravia', 'Moatize', 'Mutarara', 'Tete', 'Tsangano', 'Zumbo', 'Alto_Molocue', 'Chinde', 'Derre', 'Gile', 'Gurue', 'Ile', 'Inhassunge', 'Luabo', 'Lugela', 'Maganja_da_Costa', 'Milange', 'Mocuba', 'Mocubela', 'Molumbo', 'Mopeia', 'Morrumbala', 'Mulevala', 'Namacurra', 'Namarroi', 'Nicoadala', 'Pebane', 'Quelimane')
      S2 <- c('Cabo Delgado', 'Gaza', 'Inhambane', 'Manica', 'Maputo', 'Maputo Cidade', 'Nampula', 'Niassa', 'Sofala', 'Tete', 'Zambézia', 'Estrangeiro', 'Ancuabe', 'Balama', 'Chiúre', 'Ibo', 'Macomia', 'Mecúfi', 'Meluco', 'Metuge', 'Mocímboa da Praia', 'Montepuez', 'Mueda', 'Namuno', 'Nangade', 'Palma', 'Pemba', 'Quissanga', 'Estrangeiro', 'Bilene', 'Chibuto', 'Chicualacuala', 'Chigubo', 'Chókwè', 'Chongoene', 'Guijá', 'Limpopo', 'Mabalane', 'Mandlakazi', 'Mapai', 'Massangena', 'Massingir', 'Xai Xai', 'Funhalouro', 'Govuro', 'Homoíne', 'Inhambane', 'Inharrime', 'Inhassoro', 'Jangamo', 'Mabote', 'Massinga', 'Maxixe', 'Morrumbene', 'Panda', 'Vilankulo', 'Zavala', 'Báruè', 'Chimoio', 'Gondola', 'Guro', 'Macate', 'Machaze', 'Macossa', 'Manica', 'Mossurize', 'Sussundenga', 'Tambara', 'Vanduzi', 'Boane', 'Magude', 'Manhiça', 'Marracuene', 'Matola', 'Matutuíne', 'Moamba', 'Namaacha', 'Kamavota', 'KaMaxakeni', 'KaMphumu', 'KaMubukwana', 'KaNyaka', 'Katembe', 'Nlhamankulu', 'Angoche', 'Eráti', 'Ilha de Moçambique', 'Lalaua', 'Larde', 'Liúpo', 'Malema', 'Meconta', 'Mecubúri', 'Memba', 'Mogincual', 'Mogovolas', 'Moma', 'Monapo', 'Morrupula', 'Mossuril', 'Muecate', 'Nacala a Velha', 'Nacarôa', 'Nampula', 'Rapale', 'Ribáuè', 'Chimbonila', 'Cuamba', 'Lago', 'Lichinga', 'Majune', 'Mandimba', 'Marrupa', 'Maúa', 'Mavago', 'Mecanhelas', 'Mecula', 'Metarica', 'Muembe', 'Ngauma', 'Nipepe', 'Sanga', 'Beira', 'Búzi', 'Caia', 'Chemba', 'Cheringoma', 'Chibabava', 'Dondo', 'Gorongoza', 'Machanga', 'Marínguè', 'Marromeu', 'Muanza', 'Nhamatanda', 'Angónia', 'Cahora Bassa', 'Changara', 'Chifunde', 'Chiuta', 'Dôa', 'Macanga', 'Mágoè', 'Marara', 'Marávia', 'Moatize', 'Mutarara', 'Tete', 'Tsangano', 'Zumbo', 'Alto Molócuè', 'Chinde', 'Derre', 'Gilé', 'Gurué', 'Ile', 'Inhassunge', 'Luabo', 'Lugela', 'Maganja da Costa', 'Milange', 'Mocuba', 'Mocubela', 'Molumbo', 'Mopeia', 'Morrumbala', 'Mulevala', 'Namacurra', 'Namarroi', 'Nicoadala', 'Pebane', 'Quelimane')
      
      district_achievements <- replaceall(paao_distrital, S1, S2)
      
      dbGetQuery(pool, "DELETE FROM procava.district_achievements")
      dbWriteTable(pool, SQL("procava.district_achievements"), value =  district_achievements, append = TRUE, overwrite = FALSE, row.names = FALSE)
      
      # removeModal()
      
    })
    
    
    
    
    

    observeEvent(input$adicionar_btn, priority = 10, {
      
      SQL_df <- filtered_awpb()
      
      row_selection <- SQL_df[input$responses_table_rows_selected, "awpb_id"]
   
      qry = paste0("INSERT INTO procava.district_achieved (distrito_beneficiario, value_achieved, in_progress, update_date, awpb_id, district_status, updated_day, district_awpb)
      VALUES ('", input$distrito_beneficiario, "', ", input$value_achieved, ", ", input$in_progress, ", '", Sys.Date(), "', '", input$awpb_id, "', '", input$district_status, "', '", input$updated_day, "', '", toupper(paste0(input$awpb_id, "--", input$distrito_beneficiario)), "')
      ON CONFLICT (district_awpb) DO UPDATE SET 
      distrito_beneficiario = EXCLUDED.distrito_beneficiario,
      value_achieved = EXCLUDED.value_achieved,
      in_progress = EXCLUDED.in_progress,
      update_date = EXCLUDED.update_date,
      awpb_id = EXCLUDED.awpb_id,
      district_status = EXCLUDED.district_status,
      updated_day = EXCLUDED.updated_day,
      district_awpb = EXCLUDED.district_awpb;")
      
      dbGetQuery(pool, qry)

      updateSelectizeInput(session, "distrito_beneficiario", selected = "", "Distrito")
      updateNumericInput(session, "value_achieved", value = 0, "Realização")
    })
    
    
    
    
    observeEvent(input$adicionar_btn, {
    shinyalert("REGISTO COPIADO!", "Se tiver registe o realizado noutro distrito", type = "success")
    })

    output$contract_execution_status <- DT::renderDataTable({
      table  <- contract_ballance()
      table$end_date  <- as.Date(table$end_date)
      names(table) <- c('Contrato', 'Contratada', 'Objecto', 'Prazo do Contrato', 'Preço do Contrato', 'Factura', 'Data do Pagamento', 'Valor Facturado (MZN)', 'Cumulativo (MZN)', 'Gestor', 'Número e Contratada')
      datatable(table,  rownames = F, extensions = c('Buttons'), options = list(dom = 'Bfrtip', 
                                                                              buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print'),  pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
                                                                              columnDefs = list(list(targets = c(9:(ncol(table)-1)), visible = FALSE))))%>%
        formatCurrency(c( 'Preço do Contrato', 'Valor Facturado (MZN)', 'Cumulativo (MZN)'), '')
        # formatDate(c('Data do Pagamento'), "toLocaleDateString")
    })

    observeEvent(input$salvar_pedido, priority = 10, {

      SQL_df <- filtered_awpb()
      qry = paste0("INSERT INTO fiduciary.payment_proposals(awpb_ids, relevance, urgencia_processo, is_contract , currency, comments, contract_vs_owner, contract_levels, expense_description, e_sistafe_w_code, payment_beneficiary,
      nuit_creditado, justificativo_tipo, document_number, unidades_medida, unit_prices, quantity100, quantity30, quantity10, financier_paying, activity_started, activity_ended, applicant_nuit, process_id, process_type, submission_date, product_location, detailed, detailed_en,
      ifadloan_pct, ifadgrant_pct, ifadrpsf1_pct, ifadrpsf2_pct, beneficiaryinkind_pct, beneficiarymonetary_pct, privateinkind_pct, privatemoney_pct, governmentinkind_pct, governmentmoney_pct, contract_vs_invoice)
      VALUES ('", input$awpb_ids, "', '",

                   as.character(input$relevance_justification), "', '",
                   
                   as.character(input$urgencia_processo), "', '",
                   as.character(input$is_contract), "', '",
                   as.character(input$currency), "', '",
                   as.character(input$comments), "', '",
                   
                   input$contract_vs_owner, "', ",
                   as.numeric(input$contract_levels), ", '",
                   input$expense_description, "', ",
                   as.numeric(input$e_sistafe_w_code), ", '",
                   input$payment_beneficiary, "', ",
                   as.numeric(input$nuit_creditado), ", '",
                   as.character(input$justificativo_tipo), "', '",
                   as.character(input$document_number), "', '",
                   as.character(input$unidades_medida), "', ",
                   as.numeric(input$unit_prices), ", ",
                   as.numeric(input$quantity100), ", ",
                   as.numeric(input$quantity30), ", ",
                   as.numeric(input$quantity10), ", '",
                   input$financier_paying, "', '",

                   input$activity_started, "', '",
                   input$activity_ended, "', ",
                   as.numeric(input$applicant_nuit), ", ",
                   input$process_id, ", '",
                   input$process_type, "', '",
                   input$submission_date, "', '",
                   paste0(input$product_location, collapse = "; "), "', '",
                   input$detailed, "', '",
                   input$detailed_en, "', ",
                                       
                   input$ifadloan_pct, ", ",
                   input$ifadgrant_pct, ", ",
                   input$ifadrpsf1_pct, ", ",
                   input$ifadrpsf2_pct, ", ",
                   input$beneficiaryinkind_pct, ", ",
                   input$beneficiarymonetary_pct, ", ",
                   input$privateinkind_pct, ", ",
                   input$privatemoney_pct, ", ",
                   input$governmentinkind_pct, ", ",
                   input$governmentmoney_pct, ", '",
                   
                   toupper(ifelse(input$is_contract == "SIM", paste0(unlist(strsplit(input$contract_vs_owner, " : .*")), "--", input$justificativo_tipo, "--", input$document_number), UUIDgenerate(use.time = TRUE))),  "')")
        
      dbGetQuery(pool, qry)

      updateSelectizeInput(session, "contract_vs_owner", selected = "", "Número do Contrato")
      updateNumericInput(session, "contract_levels", value = 0, "Execução física")
      
      updateTextInput(session, "expense_description", value = '', "Detalhe da Despesa")
      
      updateSelectizeInput(session, "e_sistafe_w_code", selected = "", "Rubrica CED")
      
      updateTextInput(session, "payment_beneficiary", value = '', "Beneficiário")
      
      updateNumericInput(session, "nuit_creditado", value = 0, "NUIT a creditar")
      updateSelectizeInput(session, "justificativo_tipo", selected = "", "Documento de Suporte")
      
      updateTextInput(session, "document_number", value = "", "Número do Documento")
      
      updateSelectizeInput(session, "unidades_medida", selected = "", "Unidade")
      updateNumericInput(session, "unit_prices", value = 0, "Preço")
      updateNumericInput(session, "quantity100", value = 0, "Qty@100%")
      updateNumericInput(session, "quantity30", value = 0, "Qty@30%")
      updateNumericInput(session, "quantity10", value = 0, "Qty@10%")
      
      updateRadioButtons(session, "financier_paying", selected = 'FIDA', "Financiador")
      
      updateNumericInput(session, "ifadloan_pct", value = 0, "Crédito FIDA (%)")
      updateNumericInput(session, "ifadgrant_pct", value = 0, "Donativo FIDA (%)")
      updateNumericInput(session, "ifadrpsf1_pct", value = 0, "RPSF 1, COVID (%)")
      updateNumericInput(session, "ifadrpsf2_pct", value = 0, "RPSF 2, COVID (%)")
      updateNumericInput(session, "beneficiaryinkind_pct", value = 0, "Beneficiários em Espécie (%)")
      updateNumericInput(session, "beneficiarymonetary_pct", value = 0, "Beneficiários em Dinheiro (%)")
      updateNumericInput(session, "privateinkind_pct", value = 0, "Privados em Espécie (%)")
      updateNumericInput(session, "privatemoney_pct", value = 0, "Privados em Dinheiro (%)")
      updateNumericInput(session, "governmentinkind_pct", value = 0, "Governo em Espécie (%)")
      updateNumericInput(session, "governmentmoney_pct", value = 0, "Governo em Dinheiro (%)")

    })
    
    payments_form=modalDialog(title = "PROPOSTA DE PAGAMENTOS", footer = modalButton("Fechar"),
                              div(
                                fluidPage(
                                  fluidRow(
                                    
                                    div(style = "display: inline-block;", 
                                        selectizeInput('awpb_ids','Codigo no PAAO', choices = codes, selected = 0, width = "270px")),
                                    div(style = "display: inline-block;",
                                        dateInput('activity_started', 'Data de início', width = "100px")),
                                    div(style = "display: inline-block;", 
                                        dateInput('activity_ended', 'Data de término', width = "100px")),
                                    
                                    div(style = "display: inline-block;",
                                        selectizeInput('applicant_nuit','Proponente', choices = c('', 'Amâncio António Nhantumbo' = '106803609', 'Andércio Vitane' = '111175047', 'Augusto Oreste' = '118185331', 'Luísa Ângela Josselina Calima' = '128504222', 'Daniel Lourenço Chitupila' = '103847915', 'Dionísia Castelo Machuza Cuna' = '123200871', 'Eduardo Marcos Cuamba' = '107958584', 'Ana Crimilda Fernando Silva' = '111730814', 'Baptista Ruben Ngine Zunguze' = '103149061', 'Ernesto Abrantes Dulamo Wane' = '104922521', 'Daniel Ozias Mate' = '103659124', 'Egídio Artur Alfredo Mutimba' = '103260116', 'Esperança David Muchanga' = '109197580', 'Eugénio Nhone' = '102542762', 'Gil Estevão Nhantumbo' = '100894343', 'Jerónimo Joaquim Francisco' = '110589123', 'Joaquim Daniel Macaringue' = '300249248', 'José Sancho Cumbi' = '100870630', 'Júlio Aguiar Bila' = '106864233', 'Júlio Marcelino Macaco' = '103160987', 'Lucas Albino Chiau' = '119429897', 'Lucília Santos' = '100864665', 'Manuel Tinga Mangueze' = '100868113', 'Neide Custódio Daniel' = '102032330', 'Neila Lúcia da Conceição Manjate' = '110757891', 'Rachida Jafar Abdul' = '115695673', 'Ibraimo Assuade Assane' = '104468241', 'Tiago Tiago' = '112841822', 'Joaquim Daniel Macaringue' = '108596120', 'Ámina Amade Mussá Faquirá' = '101202429', 'Nilza Racide Abdul Adolfo' = '118151771', 'Osvaldo Lázaro Banze' = '122094545', 'Rosário Aide' = '103846501'), selected = '', multiple = FALSE, width = "350px")),
                                    div(style = "display: inline-block;",
                                        numericInput('process_id', 'Processo N.º', value = 0, width = "120px")),
                                    
                                    div(style = "display: inline-block;",
                                        selectizeInput('process_type','Tipo de processo', choices = c('', process_types), selected = '', multiple = FALSE, width = "350px")),
                                    div(style = "display: inline-block;", 
                                        dateInput('submission_date', 'Submissão', width = "120px")),
                                    
                                    div(style = "display: inline-block;",
                                        selectizeInput('product_location','Localização do Produto', choices = distritos_procava, selected = '', multiple = TRUE, width = "470")),
                                    
                                    textInput('detailed', 'Actividade Específica (Português)', width = "100%"),
                                    textInput('detailed_en', 'Actividade Específica (Inglês)', width = "100%"),
                                    
                                    textAreaInput('relevance_justification', 'Relevância da actividade', width = "100%"),
                                    
                                    radioButtons('urgencia_processo','Urgência do processo', choices = c('MUITO URGENTE', 'URGENTE', 'NORMAL'), selected = 'NORMAL', inline = TRUE),
                                    
                                    br(),
                                    div(style = "height:30px"),
                                    selectizeInput('currency','Moeda do Processo', choices = c('MZN', 'USD', 'EUR', 'ZAR', 'GBP'), selected = 'Todos', multiple = FALSE, width = "100%"),
                                    
                                    textAreaInput('comments', 'Comentários', width = "100%"),
                                    
                                    actionButton("optimize","LISTAGEM DE DESPESAS ASSOCIADAS", class = "run-button", style="color: #fff; background-color: #337ab7; border-color: #2e6da4; height: 30px; width:470px;"),
                                    
                                    radioButtons('is_contract', 'É Contrato?', choices = c('SIM', 'NÃO'), selected = "NÃO", inline = TRUE, width = "100%"),
                                    
                                    div(style = "height:30px"),
                                    div(
                                      style = "display: inline-block;", conditionalPanel(condition = "input.is_contract == 'SIM'",
                                                                                         selectizeInput('contract_vs_owner','Número do Contrato', choices = c("", contract_numbers), selected = '', multiple = FALSE, width = "470px"))),
                                    
                                    div(
                                      style = "display: inline-block;", conditionalPanel(condition = "input.is_contract == 'SIM'",
                                                                                         shiny::numericInput(inputId = "contract_levels", label = "Execução física", value =  0, width = "470px"))),
                                    
                                    div(style = "display: inline-block;",
                                        shiny::textInput(inputId = "expense_description", label = "Detalhe da Despesa", width = "470px")),
                                    
                                    div(style = "display: inline-block;",
                                        shiny::selectizeInput(inputId = "e_sistafe_w_code", 'Rubrica CED', choices = c('', 'Salários e remunerações (111100)' = '111100', 'Ajudas de custo no País (112101)' = '112101', 'Ajudas de custo fora do País (112102)' = '112102', 'Bens (121000)' = '121000', 'Combustíveis e lubrificantes (121001)' = '121001', 'Material de consumo para escritório (121005)' = '121005', 'Géneros alimentícios (121010)' = '121010', 'Serviços (122000)' = '122000', 'Comunicações em geral (122001)' = '122001', 'Passagens aéreas dentro do País (122003)' = '122002', 'Passagens fora do País (122003)' = '122003', 'Transferências  correntes  a administrações privadas (142099)' = '142099', 'Bolsas de estudo no país (143401)' = '143401', 'Construções (211000)' = '211000', 'Maquinaria, equipamento e mobiliário (212000)' = '212000', 'Meios de transporte (213000)' = '213000', 'Animais (214101)' = '214101', 'Transferências de capital a administrações privadas (222099)' = '222099', 'Demais despesas de capital (224000)' = '224000'), selected = '', multiple = FALSE, width = "470px")),
                                    
                                    
                                    div(style = "display: inline-block;", 
                                        shiny::textInput(inputId = "payment_beneficiary", label = "Beneficiário", width = "320px")
                                    ),
                                    
                                    div(style = "display: inline-block;", 
                                        shiny::numericInput(inputId = "nuit_creditado", label = "NUIT a creditar", value =  0, width = "150px")),
                                    
                                    div(style = "display: inline-block;",
                                        shiny::selectizeInput(inputId = "justificativo_tipo", 'Documento de Suporte', choices = c('', 'Declaração', 'Factura de Cobrança', 'Factura Proforma', 'Folha de Salários', 'Guia de Marcha', 'Guia de Remessa', 'Livro de Bordo', 'Relatório', 'Venda à Dinheiro', 'Outro'), selected = '', multiple = FALSE, width = "320px")),
                                    
                                    div(style = "display: inline-block;",
                                        shiny::textInput(inputId = "document_number", label = "Número do Documento", width = "150px")),
                                    
                                    div(style = "display: inline-block;",
                                        shiny::selectizeInput(inputId = "unidades_medida", 'Unidade', choices = c('Dias', 'Lum Sum', procava_measurement_units), selected = 'Todos', multiple = FALSE, width = "130px")),
                                    
                                    div(style = "display: inline-block;",
                                        numericInput('unit_prices', 'Preço', value =  0, width = "100px")),
                                    div(style = "display: inline-block;",
                                        numericInput('quantity100', 'Qty@100%', value =  0, width = "80px")),
                                    div(style = "display: inline-block;",
                                        numericInput('quantity30', 'Qty@30%', value =  0, width = "80px")),
                                    div(style = "display: inline-block;",
                                        numericInput('quantity10', 'Qty@10%', value =  0, width = "80px")),
                                    
                                    radioButtons('financier_paying', 'Financiador', choices = c('FIDA', 'COVID', 'Beneficiários', 'Governo', 'Privados'),inline = TRUE, width = "100%"),
                                    
                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'FIDA'",
                                                                                           shiny::numericInput(inputId = "ifadloan_pct", label = "Crédito FIDA (%)", value = 0, width = "240px"))),
                                    
                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'FIDA'",
                                                                                           shiny::numericInput(inputId = "ifadgrant_pct", label = "Donativo FIDA (%)", value = 0, width = "230px"))),
                                    
                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'COVID'",
                                                                                           shiny::numericInput(inputId = "ifadrpsf1_pct", label = "RPSF 1, COVID (%)", value = 0, width = "240px"))),
                                    
                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'COVID'",
                                                                                           shiny::numericInput(inputId = "ifadrpsf2_pct", label = "RPSF 2, COVID (%)", value = 0, width = "230px"))),
                                    
                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'Beneficiários'",
                                                                                           shiny::numericInput(inputId = "beneficiaryinkind_pct", label = "Beneficiários em Espécie (%)", value = 0, width = "240px"))),
                                    
                                    div(style = "display: inline-block;",conditionalPanel(condition = "input.financier_paying == 'Beneficiários'",
                                                                                          shiny::numericInput(inputId = "beneficiarymonetary_pct", label = "Beneficiários em Dinheiro (%)", value = 0, width = "230px"))),
                                    
                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'Privados'",
                                                                                           shiny::numericInput(inputId = "privateinkind_pct", label = "Privados em Espécie (%)", value = 0, width = "240px"))),
                                    
                                    div(style = "display: inline-block;",conditionalPanel(condition = "input.financier_paying == 'Privados'",
                                                                                          shiny::numericInput(inputId = "privatemoney_pct", label = "Privados em Dinheiro (%)", value = 0, width = "230px"))),
                                    
                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'Governo'",
                                                                                           shiny::numericInput(inputId = "governmentinkind_pct", label = "Governo em Espécie (%)", value = 0, width = "240px"))),
                                    
                                    div(style = "display: inline-block;",conditionalPanel(condition = "input.financier_paying == 'Governo'",
                                                                                          shiny::numericInput(inputId = "governmentmoney_pct", label = "Governo em Dinheiro (%)", value = 0, width = "230px"))),
                                    
                                    splitLayout(cellWidths = c("300px"), 
                                                actionButton("salvar_pedido", "COMETER DESPESA", style = "color: white;  background-color: blue; ", icon=icon("plus"))),
                                    
                                    actionButton("optimize","", class = "run-button", style="color: #fff; background-color: #337ab7; border-color: #2e6da4; height: 10px; width:470px;"),
                                    
                                  ),
                                  size = "l",
                                  
                                  footer = modalButton("Fechar"),
                                  easyClose = TRUE
                                )
                              )
    )
    
    
    awpb_update_form = modalDialog(title = "ACTUALIZAÇÃO DO PAAO", footer = modalButton("Fechar"),
                                   div(id=("entry_form"),
                                       tags$head(tags$style(".modal-dialog{ width:500px}")),
                                       tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
                                       fluidPage(
                                         fluidRow(
                                           
                                           splitLayout(
                                             cellWidths = c("500px"),
                                             cellArgs = list(style = "vertical-align: top"),
                                             # textInput("descricao_da_actividade", labelMandatory("Actividade"), placeholder = "")
                                             textAreaInput("descricao_da_actividade", labelMandatory("Actividade"), placeholder = "", height = 55, width = "470px")),
                                           
                                           splitLayout(
                                             cellWidths = c("125px", "400px"),
                                             cellArgs = list(style = "vertical-align: top"),
                                             selectInput("awpb_id", labelMandatory("Código"), multiple = FALSE, choices = as.character(unique(awpb_updated$awpb_id))),
                                             selectInput("situacao", labelMandatory("Situação"), multiple = FALSE, choices = awpb_situacoes, width = "340px")),
                                           
                                           textAreaInput("comentarios", "Comentários", placeholder = "", height = 100, width = "500px"),
                                           
                                           div(style = "height:30px"),
                                           radioButtons("add_distict_targets", "", choices = c("EXISTEM DADOS REAIS", "NÃO EXISTEM DADOS REAIS"), selected = "NÃO EXISTEM DADOS REAIS", inline = TRUE),
                                           # conditionalPanel(condition = "input.add_distict_targets == 'Tenho Dados Reais'",
                                           br(),
                                           div(style = "height:30px"),
                                           
                                           
                                           conditionalPanel(condition = "input.add_distict_targets == 'EXISTEM DADOS REAIS'",
                                                            actionButton("optimize","REALIZAÇÃO POR DISTRITO OU UNIDADE", class = "run-button", style="color: #fff; background-color: #337ab7; border-color: #2e6da4; height: 30px; width:465px;"),
                                                            # conditionalPanel(condition = "input.add_distict_targets == 'Tenho Dados Reais'",
                                                            selectizeInput("distrito_beneficiario", labelMandatory("Local (Digite ou escolha o Distrito ou Uniddae)"), multiple = FALSE, choices = c("", distritos_procava), selected = "", width = "465px"),
                                                            numericInput('in_progress',labelMandatory("Ainda em curso"), value = 0,  width = "465px"),
                                                            numericInput('value_achieved',labelMandatory("Já Realizados"), value = 0,  width = "465px"),
                                                            
                                                            # conditionalPanel(condition = "input.add_distict_targets == 'Tenho Dados Reais'",
                                                            selectizeInput("district_status", labelMandatory("Situação no Distrito"), multiple = FALSE, choices = c("", awpb_situacoes), selected = "",  width = "465px"),
                                                            dateInput('updated_day',label='Data da realização'),
                                                            br(),
                                                            # conditionalPanel(condition = "input.add_distict_targets == 'Tenho Dados Reais'",
                                                            actionButton("adicionar_btn", "Cometer", style = "color: white;  background-color: blue; ", icon = icon("plus")),
                                                            br(),
                                                            actionButton("optimize","", class = "run-button", style="color: #fff; background-color: #337ab7; border-color: #2e6da4; height: 10px; width:465px;")),
                                           
                                           # h4(HTML("<b>------------------------------------------------------------------------------------------------------------</b>"))),          
                                           br(),
                                           div(style = "height:30px"),
                                           conditionalPanel(condition = "input.add_distict_targets == 'EXISTEM DADOS REAIS'", helpText(labelMandatory(""), paste("Campo Obrigatório!"))),
                                           br(),
                                           splitLayout(cellWidths = c("300px"),
                                                       actionButton("submeter_actividade", "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save"))),
                                         ),
                                         
                                         footer = modalButton("Fechar"),
                                         easyClose = TRUE
                                       )
                                   )
    )
    
}

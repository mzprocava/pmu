library(bs4Dash, quietly = TRUE)
library(DT, quietly = TRUE)
library(pool, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(DBI, quietly = TRUE)
library(RPostgres, quietly = TRUE)
library(ggcharts, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(echarts4r)
library(janitor)
library(lubridate)
library(feather)
library(fst)
library(shinysurveys)
library(shinyvalidate)
library(extrafont, warn.conflicts = FALSE, quietly = T)
library(showtext, warn.conflicts = FALSE, quietly = T)
library(shinyjs, warn.conflicts = FALSE, quietly = T)
library(sodium, warn.conflicts = FALSE, quietly = T)
library(httr, warn.conflicts = FALSE, quietly = T)
library(bslib)
library(fresh)
library(splitstackshape, warn.conflicts = FALSE, quietly = T)
library(zoo)
library(data.table)
library(flextable)
library(glue)
library(shinyFiles)


bs4dash_font(size_base = "1.5rem", weight_bold = 900)
thematic::thematic_shiny(font = "auto")
options(scipen = 9999)
options(digits=15)
options(warn = 0)
e_rate = 64.46

title <- tags$a(href='https://www.google.com',
                tags$img(src="PROCAVA_LOGO.png", height = '92.5', width = '220'),
                '', target="_blank")

db <- 'mzprocava'  
host_db <- "mzprocava.c3n7ltv3naza.eu-west-1.rds.amazonaws.com"
db_port <- '5432'  
db_user <- "mzprocava"
db_password <- "GoMPROCAVA;2030"
# conn <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
pool <- dbPool(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)

onStop(function() {poolClose(pool)})

staff_choices <-  data.frame(dbGetQuery(pool, SQL("SELECT staff_name_codes from fiduciary.procava_staff")))
risk_level <- c('Muito baixo'  =  'Very Low', 'Baixo'  =  'Low', 'Médio'  =  'Medium', 'Alto'  =  'High', 'Muito alto'  =  'Very High')
garantias <- c('Garantia bancária'  =  'Bank Guarantee', 'Seguro garantia'  =  'Insurance Guarantee', 'Numerário'  =  'Cash', 'Cheque visado'  =  'Certified Cheque')
contract_status <- c('Negociação'  =  'Negotiation', 'Assinado'  =  'Signed', 'VTA ou anotação'  =  'Clearance', 'Implementação'  =  'Under implementation', 'Concluído'  =  'Completed', 'Conflito'  =  'Pending, conflict', 'Cancelado'  =  'Cancelled', 'Fechado'  =  'Closed')
moeda <- c('USD', 'ZAR', 'GBP', 'EUR', 'MZN')

instituicoes <- c("DCM", "DINAT", "DNDAF", "DNDEL", "DNDP", "DPP", "FAR,FP", "GSSA", "IIAM", "INAM", "INIR", "SETSAN")

countries <-  data.frame(dbGetQuery(pool, SQL("SELECT country from fiduciary.countries")))
countries <-  as.character(unique(countries$country))

staff_choices <- as.character(unique(staff_choices$staff_name_codes))

guarantee_issuers <-  data.frame(dbGetQuery(pool, SQL("SELECT issuer_name from fiduciary.guarantee_issuers")))
guarantee_issuers <- as.character(unique(guarantee_issuers$issuer_name))

payment_requests_choices <-  data.frame(dbGetQuery(pool, SQL("SELECT tipos_de_despesas, detailed, cost_centers FROM fiduciary.full_payments_dataset")))

tipos_de_despesas <- unique(payment_requests_choices$tipos_de_despesas)
detailed <- unique(payment_requests_choices$detailed)
cost_centers <- unique(payment_requests_choices$cost_centers)

pp_stages_pt <-  data.frame(dbGetQuery(pool, SQL("SELECT detailedstage_pt from fiduciary.pp_stages")))
pp_stages <- c('Solicitação de Manifestações de Interesse Submetido à NO', 'Solicitação de Manifestações de Interesses Aprovada', 'Solicitação de Manifestações de Interesses Publicada', 'Manifestações de Interesses Submetidas', 'Relatório de Manifestações de Interesse Submetido à NO', 'Relatório de Manifestações de Interesse Aprovado', 'Convite para Apresentação de Propostas Submetido à NO', 'Convite para Apresentação de Propostas Aprovado', 'Convite para Apresentação de Propostas Publicado', 'Propostas Submetidas', 'Relatório de Propostas Técnicas Submetido à NO', 'Relatório de Propostas Técnicas Aprovado', 'Propostas Financeiras Abertas', 'Relatório Combinado Submetido à NO', 'Relatório Combinado Aprovado', 'Notificação da Intenção de Adjudicação', 'Adjudicação do Contrato', 'Negociação do Contrato', 'Proposta do Contrato Submetido à Não Objecção', 'Proposta do Contrato Aprovado', 'Contrato Assinado', 'Contrato visado', 'Contrato em Implementação', 'Contrato encerrado', 'Não iniciado', 'Dossier em litígio')
pp_stages_colours <- c('#ffa100', '#fbad7a', '#fbee7a', '#d0c229', '#d0a129', '#d0b529', '#e4f808', '#e0f808', '#b6f808', '#08ebf8', '#08f8d7', '#08f8be', '#9df808', '#8cf808', '#5ef808', '#69ff71', '#4dfc03', '#49e507', '#2ce507', '#0cd337', '#35aa07', '#0faa07', '#07aa10', '#058a0c', '#ff0f00', '#fb7a87')

pp_dataset <-  DBI::dbGetQuery(pool, "SELECT * from fiduciary.procurement_dossiers")
pp_methods <- read_feather('procurement_deadlines.feather') %>% select(method_name_pt)
pp_method_names_pt <- as.list(unique(pp_methods$method_name_pt))
codes <- as.list(unique(pp_stages_pt$detailedstage_pt))
pp_responsibles <- as.list(unique(pp_dataset$responsible))  

labelMandatory <- function(label) {tagList(label,span("*", class = "mandatory_star"))}
appCSS <- ".mandatory_star { color: red; }"

disbursed <- dbGetQuery(pool, "SELECT contravalor_mzn, disbursed_usd from fiduciary.withdrawal_applications")
procurement_view <- dbGetQuery(pool, "SELECT * FROM fiduciary.procurement_view")

components_design <- read_feather("component_years_project.feather")  %>% select(subcomponent = components, total)
awpb_updated <-  dbGetQuery(pool, "SELECT internal_responsible, awpb_id from procava.awpb_updates")
pagamentos_aprovados <- dbGetQuery(pool, "SELECT * from fiduciary.full_approved_payments")

paao_granulado <-  read_feather('granular_awpb_2022.feather')
procava_staff <-  dbGetQuery(pool, "SELECT awpb_role from fiduciary.procava_staff")

cost_tab <- read_feather('cost_tab.feather')
procava_cost_tabs <-  cost_tab
ced_codes <- read_feather("e_sistafe.feather") %>% select(ced = esistafe_key, e_sistafe_w_code)
paid_set <- pagamentos_aprovados
e_sistafe  <-  ced_codes %>% dplyr::select(esistafe_key=ced, description_e_sistafe=e_sistafe_w_code)
paid_set$quarters <- quarters(as.Date(paid_set$payment_date))

contractrecords_df <- read_feather('contractrecords_df.feather')

costtabs <- read_feather('costtabs.feather')

granular_awpb_2022  <- paao_granulado
components_design <- read_feather("component_years_project.feather") %>% select(subcomponent = components, PDR=total)
cost_tabs_pdr <- read_fst("summarycosttables.fst")

PDR_categories <- cost_tabs_pdr %>% dplyr::filter(class == "categories") %>% select(pdr_category = Expenditure, PDR = total_cost)
PDR_categories$pdr_category[PDR_categories$pdr_category == "Credit, Guarantee Funds"] <- "Credit Guarantee Funds"
PDR_categories$pdr_category[PDR_categories$pdr_category == "Training"] <- "Trainings"
PDR_categories$pdr_category[PDR_categories$pdr_category == "Workshop"] <- "Workshops"
PDR_categories$pdr_category[PDR_categories$pdr_category == "Works"] <- "Civil Works"

detailed_pp_stages <- as.list(unique(pp_stages_pt$detailedstage_pt))
pp_responsibles <- as.list(unique(pp_dataset$responsible)) 

codes <- as.list(unique(awpb_updated$awpb_id))
responsaveis <- as.list(unique(awpb_updated$internal_responsible))
management_units <- sort(c("URGPS", "UNGP", "URGPC", "URGN", "UPGPN"))

labelMandatory <- function(label) {tagList(label,span("*", class = "mandatory_star"))}

appCSS <- ".mandatory_star { color: red; ,}"
awpb_situacoes <- c("Não iniciada (atrasada)", "Não iniciada (dentro do prazo)", "Iniciada (Execução < 50%)", "Estado avançado (50% < Execução < 75%)", "Quase concluída (75% < Execução < 100%)", "Concluída  (Execução >= 100%)", "Cancelada")

callback <- c(
  "var id = $(table.table().node()).closest('.datatables').attr('id');",
  "$.contextMenu({",
  "  selector: '#' + id + ' td.factor input[type=text]',",
  "  trigger: 'hover',",
  "  build: function($trigger, e){",
  "    var levels = $trigger.parent().data('levels');",
  "    if(levels === undefined){",
  "      var colindex = table.cell($trigger.parent()[0]).index().column;",
  "      levels = table.column(colindex).data().unique();",
  "    }",
  "    var options = levels.reduce(function(result, item, index, array){",
  "      result[index] = item;",
  "      return result;",
  "    }, {});",
  "    return {",
  "      autoHide: true,",
  "      items: {",
  "        dropdown: {",
  "          name: 'Edit',",
  "          type: 'select',",
  "          options: options,",
  "          selected: 0",
  "        }",
  "      },",
  "      events: {",
  "        show: function(opts){",
  "          opts.$trigger.off('blur');",
  "        },",
  "        hide: function(opts){",
  "          var $this = this;",
  "          var data = $.contextMenu.getInputValues(opts, $this.data());",
  "          var $input = opts.$trigger;",
  "          $input.val(options[data.dropdown]);",
  "          $input.trigger('change');",
  "        }",
  "      }",
  "    };",
  "  }",
  "});"
)

createdCell <- function(levels){
  if(missing(levels)){
    return("function(td, cellData, rowData, rowIndex, colIndex){}")
  }
  quotedLevels <- toString(sprintf("\"%s\"", levels))
  c(
    "function(td, cellData, rowData, rowIndex, colIndex){",
    sprintf("  $(td).attr('data-levels', '[%s]');", quotedLevels),
    "}"
  )
}


procava_measurement_units <- read_fst("procava_units.fst")
procava_measurement_units <- unique(as.character(procava_measurement_units$unit_pt))

process_types <- c('Reembolso de fundos dos técnicos', 'Pagamento de salários e remunerações', 'Pagamento de despesas de logística de viagens', 'Pagamento de fundos cobrados por fornecedores', 'Adiantamento de fundos aos técnicos', 'Adiantamento de fundos aos fornecedores', 'Pagamento de despesas de realização de evento')


full_payments <- dbGetQuery(pool, "SELECT * FROM fiduciary.full_approved_payments")

procava_payments <- full_payments

procava_payments$quarters_paid <- as.yearqtr(as.Date(procava_payments$submission_date, "%m/%d/%Y"))
procava_payments <- concat.split(data = procava_payments, split.col = which(colnames(procava_payments) == "quarters_paid"), sep = " ", drop = FALSE)
setnames(procava_payments, c("quarters_paid_1", "quarters_paid_2"), c("year_paid", "quarter_paid"))

df <- read_feather('payments_survey.feather')
extendInputType("date",{shiny::dateInput(inputId = surveyID(),value = Sys.Date(), label = surveyLabel(), min = Sys.Date()-1100, max = Sys.Date()+1900)})
extendInputType("inlineRadioButtons", {shiny::radioButtons(inputId = surveyID(), label = surveyLabel(),selected = character(0),choices = surveyOptions(),inline = TRUE)})
extendInputType("commentsInputs", {shiny::textAreaInput("caption", "Caption", "Observações", width = "100%")})
extendInputType("exchange_rate", {shiny::numberInput(inputId = "exchange_rate", label = "Câmbio para o Dólar", value = 64.46,
                                                     min = 0, max = 1000000, step = 0.01, placeholder = NULL, width = NULL)})
extendInputType("checkboxinput", {
  shiny::checkboxGroupInput(inputId = surveyID(),label = surveyLabel(),choices = surveyOptions(),selected = NULL,
                            inline = FALSE,width = NULL,choiceNames = NULL,choiceValues = NULL)})

extendInputType("contract_choices", {
  db <- 'mzprocava'  
  host_db <- "mzprocava.c3n7ltv3naza.eu-west-1.rds.amazonaws.com"
  db_port <- '5432'  
  db_user <- "mzprocava"
  db_password <- "GoMPROCAVA;2030"
  pool <- dbPool(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
  contratos <- dbGetQuery(pool, "SELECT contract_number FROM fiduciary.external_contract_ballance")
  contract_choices <- unique(as.character(contratos$contract_number))
  shiny::selectizeInput(inputId = surveyID(),label = surveyLabel(), choices = c("", contract_choices))
})

extendInputType("staff_choices", {
  db <- 'mzprocava'  
  host_db <- "mzprocava.c3n7ltv3naza.eu-west-1.rds.amazonaws.com"
  db_port <- '5432'  
  db_user <- "mzprocava"
  db_password <- "GoMPROCAVA;2030"
  pool <- dbPool(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
  staff_codes <- dbGetQuery(pool, "SELECT staff_name_codes FROM fiduciary.procava_staff")
  staff_codes <- unique(as.character(staff_codes$staff_name_codes))
  shiny::selectizeInput(inputId = surveyID(),label = surveyLabel(), choices = c("", staff_codes))
})

extendInputType("awpb_codes_choices", {
  awpb_choices <- read_feather("awpb_and_costab_codes.feather", columns = c("activity_choices"))
  awpb_choices <- unique(as.character(awpb_choices$activity_choices))
  shiny::selectizeInput(inputId = surveyID(),label = surveyLabel(), choices = c("", awpb_choices))
})

extendInputType("unit_choices", {
  units <- read_fst("procava_units.fst")
  units_choices <- unique(as.character(units$unit_pt))
  shiny::selectizeInput(inputId = surveyID(),label = surveyLabel(), choices = c("", units_choices))
})

extendInputType("district_choices", {
  district_choices <- read_feather('odk_options.feather', columns = c("list_name", "label_portuguese")) %>% filter(list_name == "distrito_new")
  district_choices <- unique(as.character(district_choices$label_portuguese))
  shiny::selectizeInput(inputId = surveyID(),label = surveyLabel(), choices = c("", district_choices), multiple = TRUE)
})

variaveis_contratos <- c('Método de procurement' = "proc_method",
                         'Unidade Gestora (UGB)' = "cost_center",
                         'País da contratada' = "supplier_country",
                         'Gestor do Contrato' = "contract_manager",
                         'Infraestrutura' = "infrastructure",
                         'Categoria de procurement' = "procurement_type",
                         'Revisão' = "review",
                         'Situação do contrato' = "contract_status",
                         'Nível de risco' = "risk_flag")

valores_contratos <- c("Valor revisto" = "revised_ammount", "Valor pago" = "ammount_paid")

valores_contratos <- c("Valor revisto" = "revised_ammount", "Valor pago" = "value_paid")

contract_numero <-  DBI::dbGetQuery(pool, "SELECT contract_number FROM fiduciary.external_contract_ballance")
contract_numbers <-  unique(contract_numero$contract_number)

distritos_procava <- c("UNGP", "URGPS", "URGPC", "URGPN", "UPGPN", 'Alto Molócuè' = 'Alto_Molocue', 'Ancuabe' = 'Ancuabe', 'Angoche' = 'Angoche', 'Angónia' = 'Angonia', 'Balama' = 'Balama', 'Báruè' = 'Barue', 'Beira' = 'Beira', 'Bilene' = 'Bilene', 'Boane' = 'Boane', 'Búzi' = 'Buzi', 'Cahora Bassa' = 'Cahora_Bassa', 'Caia' = 'Caia', 'Changara' = 'Changara', 'Chemba' = 'Chemba', 'Cheringoma' = 'Cheringoma', 'Chibabava' = 'Chibabava', 'Chibuto' = 'Chibuto', 'Chicualacuala' = 'Chicualacuala', 'Chifunde' = 'Chifunde', 'Chigubo' = 'Chigubo', 'Chimbonila' = 'Chimbonila', 'Chimoio' = 'Chimoio', 'Chinde' = 'Chinde', 'Chiúre' = 'Chiure', 'Chiuta' = 'Chiuta', 'Chókwè' = 'Chokwe', 'Chongoene' = 'Chongoene', 'Cuamba' = 'Cuamba', 'Derre' = 'Derre', 'Dôa' = 'Doa', 'Dondo' = 'Dondo', 'Eráti' = 'Erati', 'Estrangeiro' = 'Estrangeiro', 'Funhalouro' = 'Funhalouro', 'Gilé' = 'Gile', 'Gondola' = 'Gondola', 'Gorongoza' = 'Gorongoza', 'Govuro' = 'Govuro', 'Guijá' = 'Guija', 'Guro' = 'Guro', 'Gurué' = 'Gurue', 'Homoíne' = 'Homoine', 'Ibo' = 'Ibo', 'Ile' = 'Ile', 'Ilha de Moçambique' = 'Ilha_de_Mocambique', 'Inhambane' = 'Inhambane', 'Inharrime' = 'Inharrime', 'Inhassoro' = 'Inhassoro', 'Inhassunge' = 'Inhassunge', 'Jangamo' = 'Jangamo', 'Kamavota' = 'Kamavota', 'KaMaxakeni' = 'KaMaxakeni', 'KaMphumu' = 'KaMphumu', 'KaMubukwana' = 'KaMubukwana', 'KaNyaka' = 'KaNyaka', 'Katembe' = 'Katembe', 'Lago' = 'Lago', 'Lalaua' = 'Lalaua', 'Larde' = 'Larde', 'Lichinga' = 'Lichinga', 'Limpopo' = 'Limpopo', 'Liúpo' = 'Liupo', 'Luabo' = 'Luabo', 'Lugela' = 'Lugela', 'Mabalane' = 'Mabalane', 'Mabote' = 'Mabote', 'Macanga' = 'Macanga', 'Macate' = 'Macate', 'Machanga' = 'Machanga', 'Machaze' = 'Machaze', 'Macomia' = 'Macomia', 'Macossa' = 'Macossa', 'Maganja da Costa' = 'Maganja_da_Costa', 'Mágoè' = 'Magoe', 'Magude' = 'Magude', 'Majune' = 'Majune', 'Malema' = 'Malema', 'Mandimba' = 'Mandimba', 'Mandlakazi' = 'Mandlakazi', 'Manhiça' = 'Manhica', 'Manica' = 'Manica', 'Mapai' = 'Mapai', 'Marara' = 'Marara', 'Marávia' = 'Maravia', 'Marínguè' = 'Maringue', 'Marracuene' = 'Marracuene', 'Marromeu' = 'Marromeu', 'Marrupa' = 'Marrupa', 'Massangena' = 'Massangena', 'Massinga' = 'Massinga', 'Massingir' = 'Massingir', 'Matola' = 'Matola', 'Matutuíne' = 'Matutuine', 'Maúa' = 'Maua', 'Mavago' = 'Mavago', 'Maxixe' = 'Maxixe', 'Mecanhelas' = 'Mecanhelas', 'Meconta' = 'Meconta', 'Mecubúri' = 'Mecuburi', 'Mecúfi' = 'Mecufi', 'Mecula' = 'Mecula', 'Meluco' = 'Meluco', 'Memba' = 'Memba', 'Metarica' = 'Metarica', 'Metuge' = 'Metuge', 'Milange' = 'Milange', 'Moamba' = 'Moamba', 'Moatize' = 'Moatize', 'Mocímboa da Praia' = 'Mocimboa_da_Praia', 'Mocuba' = 'Mocuba', 'Mocubela' = 'Mocubela', 'Mogincual' = 'Mogincual', 'Mogovolas' = 'Mogovolas', 'Molumbo' = 'Molumbo', 'Moma' = 'Moma', 'Monapo' = 'Monapo', 'Montepuez' = 'Montepuez', 'Mopeia' = 'Mopeia', 'Morrumbala' = 'Morrumbala', 'Morrumbene' = 'Morrumbene', 'Morrupula' = 'Morrupula', 'Mossuril' = 'Mossuril', 'Mossurize' = 'Mossurize', 'Muanza' = 'Muanza', 'Muecate' = 'Muecate', 'Mueda' = 'Mueda', 'Muembe' = 'Muembe', 'Mulevala' = 'Mulevala', 'Mutarara' = 'Mutarara', 'Nacala a Velha' = 'Nacala_a_Velha', 'Nacarôa' = 'Nacaroa', 'Namaacha' = 'Namaacha', 'Namacurra' = 'Namacurra', 'Namarroi' = 'Namarroi', 'Nampula' = 'Nampula', 'Namuno' = 'Namuno', 'Nangade' = 'Nangade', 'Ngauma' = 'Ngauma', 'Nhamatanda' = 'Nhamatanda', 'Nicoadala' = 'Nicoadala', 'Nipepe' = 'Nipepe', 'Nlhamankulu' = 'Nlhamankulu', 'Palma' = 'Palma', 'Panda' = 'Panda', 'Pebane' = 'Pebane', 'Pemba' = 'Pemba', 'Quelimane' = 'Quelimane', 'Quissanga' = 'Quissanga', 'Rapale' = 'Rapale', 'Ribáuè' = 'Ribaue', 'Sanga' = 'Sanga', 'Sussundenga' = 'Sussundenga', 'Tambara' = 'Tambara', 'Tete' = 'Tete', 'Tsangano' = 'Tsangano', 'Vanduzi' = 'Vanduzi', 'Vilankulo' = 'Vilankulo', 'Xai Xai' = 'Xai_Xai', 'Zavala' = 'Zavala', 'Zumbo' = 'Zumbo')



staff_nuit <- c('Amâncio António Nhantumbo' = '106803609', 'Andércio Vitane' = '111175047', 'Augusto Oreste' = '118185331', 'Luísa Ângela Josselina Calima' = '128504222', 'Daniel Lourenço Chitupila' = '103847915', 'Dionísia Castelo Machuza Cuna' = '123200871', 'Eduardo Marcos Cuamba' = '107958584', 'Ana Crimilda Fernando Silva' = '111730814', 'Baptista Ruben Ngine Zunguze' = '103149061', 'Ernesto Abrantes Dulamo Wane' = '104922521', 'Daniel Ozias Mate' = '103659124', 'Egídio Artur Alfredo Mutimba' = '103260116', 'Esperança David Muchanga' = '109197580', 'Eugénio Nhone' = '102542762', 'Gil Estevão Nhantumbo' = '100894343', 'Jerónimo Joaquim Francisco' = '110589123', 'Joaquim Daniel Macaringue' = '300249248', 'José Sancho Cumbi' = '100870630', 'Júlio Aguiar Bila' = '106864233', 'Júlio Marcelino Macaco' = '103160987', 'Lucas Albino Chiau' = '119429897', 'Lucília Santos' = '100864665', 'Manuel Tinga Mangueze' = '100868113', 'Neide Custódio Daniel' = '102032330', 'Neila Lúcia da Conceição Manjate' = '110757891', 'Rachida Jafar Abdul' = '115695673', 'Ibraimo Assuade Assane' = '104468241', 'Tiago Tiago' = '112841822', 'Joaquim Daniel Macaringue' = '108596120', 'Ámina Amade Mussá Faquirá' = '101202429', 'Nilza Racide Abdul Adolfo' = '118151771', 'Osvaldo Lázaro Banze' = '122094545', 'Rosário Aide' = '103846501')


# awpb_update_form <showModal(

# )
# }



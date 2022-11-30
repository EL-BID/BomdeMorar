
consultaUI <- function(id, SM, subsidio_min, subsidio_max, aluguel_max) {
    ns = NS(id)
    tagList(
        fluidRow(
            box(width = 4,
                title = tags$b("Dados para Consulta"), 
                status = "primary",
                solidHeader = F, 
                collapsible = F, 
                #br(),
                "Digite os dados do usuário",
                br(),
                br(),
                textInput(
                    inputId = ns("cpf"), 
                    label = "CPF", 
                    placeholder = "XXX.XXX.XXX-XX"
                ),
                fluidRow(
                    column(
                        width = 6,
                        actionButton(
                            inputId = ns("pesquisar"), 
                            label = "Pesquisar"
                        )
                    ),
                    column(
                        width = 6,
                        actionButton(
                            inputId = ns("limpar"),
                            label = "Limpar"
                        )
                    )
                )
            ),
            box(width = 8,
                status = "primary",
                solidHeader = F, 
                collapsible = F, 
                title = tags$b("Dados do Solicitante"),
                fluidRow(
                    infoBoxOutput(outputId = ns("nome_benef"), width = 6),
                    infoBoxOutput(outputId = ns("nis_benef"), width = 6)
                ),
                fluidRow(
                    infoBoxOutput(outputId = ns("endereco_fam_benef"), width = 6),
                    infoBoxOutput(outputId = ns("num_integ_fam_benef"), width = 6)
                ),
                fluidRow(
                    infoBoxOutput(outputId = ns("renda_total_fam_sm_benef"), width = 6),
                    infoBoxOutput(outputId = ns("renda_media_fam_sm_benef"), width = 6)
                )
            )
        ),
        fluidRow(
            box(title = tags$b("Elegibilidade ao Programa"),
                width = 12,
                status = "primary",
                solidHeader = F, 
                collapsible = F, 
                # column(
                #     width = 6,
                #     offset = 3,
                #     align = "center",
                #     valueBoxOutput(outputId = ns("approval_box"), width=NULL)
                # ),
                valueBoxOutput(outputId = ns("approval_box"), width = 4),
                infoBoxOutput(outputId = ns("modalidade"), width = 4),
                infoBoxOutput(outputId = ns("n_apts"), width = 4)
            )
        ),
        fluidRow(
            box(width = 12,
                status = "primary",
                solidHeader = F, 
                collapsible = F, 
                title = tags$b("Informações"),
                infoBoxOutput(outputId = ns("comp_renda_max"), width = 4),
                infoBoxOutput(outputId = ns("valor_aluguel_min"), width = 4),
                infoBoxOutput(outputId = ns("valor_aluguel_max"), width = 4)
            )
        ),
        fluidRow(
            tabBox(
                width = 12,
                tabPanel(
                  title = tags$b("Mapa"),
                  leafletOutput(outputId = ns("mapa"))
                ),
                tabPanel(
                    title = tags$b("Lista de Imóveis"),
                    dataTableOutput(outputId = ns("resultado"))
                )
            )
        )
    )
}

consultaServer <- function(id, SM, subsidio_min, subsidio_max, aluguel_max) {
    moduleServer(
        id,
        function(input, output, session) {
            # Limpar sessao:
            observeEvent(input$limpar, {
                updateTextInput(inputId = "cpf", value = "")
                shinyjs::click(id = "pesquisar")
            })
            
            # 
            out <- eventReactive(input$pesquisar, {
                req(input$cpf != "")
                # Get CPF and clean it:
                cpf = input$cpf
                cpf = str_pad(cpf, 11, "left", "0")
                # Check elegibilidade:
                res = checkElegibility_CPF(cpf)
                # Se for elegível:
                if( res$resultado == "elegivel" ) {
                    # calculando modalidade do programa e subsídio:
                    res$infoFamily = playProgramaBomDeMorar(
                        infoFamily = res$infoFamily, 
                        SM = SM, 
                        SUBSIDIO_MIN = subsidio_min, 
                        SUBSIDIO_MAX = subsidio_max, 
                        ALUGUEL_MAX = aluguel_max
                    )
                    # calculando lista de imóveis:
                    res$listaImoveis = listaImoveisDisp(
                        cr = res$infoFamily$CR[[1]],
                        aluguel_up = res$infoFamily$aluguel_max[[1]],
                        aluguel_lw = res$infoFamily$aluguel_min[[1]]
                    )
                }
                res
            })
            
            ############################# OUTPUT ###############################
            observeEvent(out()$resultado, {
                if( out()$resultado == "inelegivel") {
                    sendSweetAlert(
                        session = session,
                        title = "Inelegível para o programa Bom de Morar",
                        text = tags$span(
                            tags$h3("Candidato Inelegível",
                                    style = "color: steelblue;"),
                            tags$b("Dispositivo:"),
                            tags$br(),
                            str_replace_all(out()$dispositivo, "[[:space:]]", " "),
                            tags$br(),
                            tags$b(out()$motivo)
                        ),
                        html = TRUE
                        #type = "error"
                    )
                    shinyjs::reset(id = "cpf")
                    #shinyjs::click(id = "pesquisar")
                }
            })
            
            ## InfoBox Nome do beneficiario:
            output$nome_benef <- renderInfoBox({
                req( out()$resultado == "elegivel" )
                dt = out()$infoFamily |> 
                    filter(p.num_cpf_pessoa == isolate(input$cpf))
                infoBox(
                  "Nome", 
                  dt$p.nom_pessoa[[1]],
                  icon = icon("user-circle", verify_fa = F),
                  width = 8,
                  color = "purple"
                )
            })
            ## InfoBox Numero do NIS
            output$nis_benef <- renderInfoBox({
                req( out()$resultado == "elegivel" )
                dt = out()$infoFamily |> 
                    filter(p.num_cpf_pessoa == isolate(input$cpf))
                infoBox(
                    "NIS", dt$p.num_nis_pessoa_atual[[1]],
                    icon = icon("list", lib = "glyphicon"),
                    color = "blue"
                )
            })
             ## InfoBox Código da Família no CAD Único
            output$cod_familiar_benef <- renderInfoBox({
                req( out()$resultado == "elegivel" )
                dt = out()$infoFamily |> 
                    filter(p.num_cpf_pessoa == isolate(input$cpf))
                infoBox(
                    "Código familiar (CADÚnico)", dt$d.cod_familiar_fam[[1]],
                    icon = icon("list", lib = "glyphicon"),
                    color = "blue"
                )
            })
            ## InfoBox Endereço
            output$endereco_fam_benef <- renderInfoBox({
                req( out()$resultado == "elegivel" )
                dt = out()$infoFamily |> 
                    filter(p.num_cpf_pessoa == isolate(input$cpf))
                tipo_logradouro = dt$d.nom_tip_logradouro_fam[[1]]
                nome_logradouro = dt$d.nom_logradouro_fam[[1]]
                numero_logradouro = dt$d.num_logradouro_fam[[1]]
                nome_bairro = dt$d.nom_localidade_fam[[1]]
                endereco = paste0(
                    tipo_logradouro, " ",
                    nome_logradouro, ", ",
                    numero_logradouro, ". ",
                    nome_bairro
                )
                infoBox(
                    "Endereço", endereco,
                    icon = icon("home", lib = "glyphicon"),
                    color = "green"
                )
            })
            ## InfoBox Número de Integrantes na Família
            output$num_integ_fam_benef <- renderInfoBox({
                req( out()$resultado == "elegivel" )
                # Calculando numero de integrantes na familia:
                n_integrantes <- n_distinct(out()$infoFamily$p.num_nis_pessoa_atual)
                infoBox(
                    "Número de Integrantes na Família",
                    n_integrantes,
                    icon = icon("users", lib = "font-awesome", verify_fa = F),
                    color = "yellow"
                )
            })
            ## InfoBox Renda Familiar:
            output$renda_total_fam_sm_benef <- renderInfoBox({
                req( out()$resultado == "elegivel" )
                
                infoBox(
                    "Renda Familiar",
                    paste0("R$ ", out()$infoFamily$d.vlr_renda_total_fam[[1]]),
                    #paste0(100*vlr_renda_total_fam_SM(), "%"),
                    icon = icon("wallet", lib = "font-awesome", verify_fa = F),
                    color = "green"
                )
            })
            ## ValueBox Elegibilidade
            output$approval_box <- renderValueBox({
                req( out()$resultado == "elegivel" )
                valueBox(
                    "Elegível", "",
                    icon = icon("thumbs-up", lib = "font-awesome", verify_fa = F),
                    color = "green"
                )
            })
            ## InfoBox com modalidade do programa:
            output$modalidade <- renderInfoBox({
                req( out()$resultado == "elegivel" )
                infoBox(
                    "Modalidade do Programa Bom de Morar", 
                    out()$infoFamily$modalidade[[1]],
                    icon = icon("house-user", lib = "font-awesome", verify_fa = F),
                    color = "blue"
                 )
             })
            
            # InfoBox: numero de apt disponívels:
            output$n_apts = renderInfoBox({
                req( out()$resultado == "elegivel" )
                infoBox(
                    "Número de Imóveis Disponíveis",
                    n_distinct(out()$listaImoveis$apt_id),
                    icon = icon("building", lib = "font-awesome", verify_fa = F),
                    color = "blue"
                 )
            })
            
            # InfoBox: Comprometimento de renda máximo familiar com aluguel:
            output$comp_renda_max <- renderValueBox({
                req( out()$resultado == "elegivel" )
                infoBox(
                    "Comprometimento de Renda Familiar",
                    paste0("R$ ", out()$infoFamily$CR[[1]] %>% round(2)),
                    icon = icon("money-bill", lib = "font-awesome", verify_fa = F),
                    color = "green"
                )
            })
            # InfoBox: Valor Aluguel Máximo
            output$valor_aluguel_max <- renderInfoBox({
                req( out()$resultado == "elegivel" )
                infoBox(
                    "Valor Aluguel (Máximo)",
                    paste0("R$ ", out()$infoFamily$aluguel_max[[1]]),
                    icon = icon("money-bill", lib = "font-awesome", verify_fa = F),
                    color = "yellow"
                )
            })
            # InfoBox: Valor Aluguel Mínimo
            output$valor_aluguel_min <- renderInfoBox({
                req( out()$resultado == "elegivel" )
                infoBox(
                    "Valor Aluguel (Mínimo)",
                    paste0("R$ ", out()$infoFamily$aluguel_min[[1]]),
                    icon = icon("money-bill", lib = "font-awesome", verify_fa = F),
                    color = "yellow"
                )
            })
            
            # Tabela com os imóveis
            output$resultado <- renderDataTable({
                req( out()$resultado == "elegivel" )
                out()$listaImoveis
            })
            
            # Mapa:
            output$mapa <- renderLeaflet({
                req( out()$resultado == "elegivel" )
                out()$listaImoveis %>%
                   plotMap()
            })
        }
    )
}


consultaApp = function() {
    ui = fluidPage(
        shinyjs::useShinyjs(),
        useSweetAlert(),
        consultaUI(id = "id1", SM = 1212, subsidio_min = 50, subsidio_max = 600, aluguel_max = 1000)
    )
    server = function(input, output, session){
        consultaServer(id = "id1", SM = 1212, subsidio_min = 50, subsidio_max = 600, aluguel_max = 1000)
    }
    shinyApp(ui, server)
}

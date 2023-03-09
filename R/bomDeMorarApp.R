#' Bom de Morar UI
#' 
#' @description 
#' User Interface (UI) for the Proof of Concept Bom de Morar App.
#' 
#' @details 
#' This function only provides  the User Interface (frontend)
#' of the application. 
#'
#' @return the HTML for the frontend.
#' 
#' @import shiny
#' @import shinydashboard
#' @import htmltools
#' 
#' @export

app_ui = function() {
    
    # GLOBAL VAR:
    SM = 1212
    SUBSIDIO_MIN = 50
    SUBSIDIO_MAX = 600
    ALUGUEL_MAX = 1000
    
    # header:
    header <- shinydashboardPlus::dashboardHeader(
        title = "Programa Bom de Morar",
        titleWidth = 400,
        tags$li(a(href = 'https://www2.recife.pe.gov.br/',
                  img(src = 'dual_logo3.png',
                      title = "Programa Bom de Morar PoC", height = "50px"),
                  style = "padding-top:10px; padding-bottom:10px;"),
                class = "dropdown")
    )
    
    # sidebar:
    sidebar <- shinydashboardPlus::dashboardSidebar(
        collapsed = F,
        sidebarMenu(
            menuItem(
                text = "Informações", 
                tabName = "informacoes",
                icon = icon("info")
            ),
            menuItem(
                text = "Consulta",
                tabName = "consulta",
                icon = icon("search")
            ),
            menuItem(
                text = "Feedback",
                tabName = "feedback",
                icon = icon('envelope')
            )
        )
    )
    
    # body:
    body <- dashboardBody(
        shinyWidgets::useSweetAlert(),
        shinyjs::useShinyjs(),
        includeCSS("www/custom.css"),
        tabItems(
            tabItem(
                tabName = "informacoes",
                fluidRow(
                    box(width = 12,
                        status = "primary",
                        solidHeader = F, 
                        collapsible = F, 
                        div(id = "info-programa",
                            informacoesUI("id1"))
                    )
                )
            ),
            tabItem(
                tabName = "consulta",
                consultaUI(
                    "id1", 
                    SM = SM, 
                    subsidio_min = SUBSIDIO_MIN, 
                    subsidio_max = SUBSIDIO_MAX, 
                    aluguel_max = ALUGUEL_MAX
                )
            )
        )
    )
    
    # FrontEnd:
    ui <- shinydashboardPlus::dashboardPage(
        header = header, 
        sidebar = sidebar, 
        body = body
    )
    
    return(ui)
}

#' Bom de Morar Server
#' 
#' @description 
#' Backend for the Proof of Concept Bom de Morar App.
#' 
#' @details 
#' This function only provides the backend of the application. 
#'
#' @export


app_server = function() {

    # Backend
    server <- function(input, output, session) {
        consultaServer(
            "id1", 
            SM = SM, 
            subsidio_min = SUBSIDIO_MIN, 
            subsidio_max = SUBSIDIO_MAX,
            aluguel_max = ALUGUEL_MAX
        )
    }
    
    server
}

#' Bom de Morar App
#' 
#' @export

bomdeMorarApp = function() {
    shinyApp(ui = app_ui(), server = app_server())
}

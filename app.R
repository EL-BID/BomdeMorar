# Attaching packages:
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(data.table)
library(tidyverse)
library(lubridate)
library(leaflet)

# sourcing modules:
source("R/consulta_module.R")
source("R/informacoes_module.R")
source("R/helper_functions.R")

# GLOBAL VAR:
SM = 1212
SUBSIDIO_MIN = 50
SUBSIDIO_MAX = 600
ALUGUEL_MAX = 1000

# header:
header <- dashboardHeader(
    title = "Programa Bom de Morar",
    titleWidth = 400,
    tags$li(a(href = 'https://www2.recife.pe.gov.br/',
              img(src = 'dual_logo3.png',
                  title = "Programa Bom de Morar PoC", height = "50px"),
              style = "padding-top:10px; padding-bottom:10px;"),
            class = "dropdown")
)

# sidebar:
sidebar <- dashboardSidebar(
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
    useSweetAlert(),
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
ui <- dashboardPage(
    header = header, 
    sidebar = sidebar, 
    body = body
)

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

shinyApp(ui, server)

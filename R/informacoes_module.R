#' Informações UI
#'
#' @import shiny
#'
#' @export

informacoesUI = function(id) {
    ns = NS(id)
    tagList(
        h1("Programa Bom de Morar"),
        tags$p("O Programa Bom de Morar tem por objetivo prover uma alternativa de
        solução habitacional para famílias de baixa renda, enquanto mantidas as
        condições de elegibilidade previstas nesta lei, 
        através da concessão de subsídio destinado à locação de 
               imóveis a preços acessíveis."
        ),
        
        tags$br(),
        
        h1("Elegibilidade e Condições de Adesão"),
        
        tags$p(
        "Art. 2° É elegível para o Programa Bom de Morar a família que atender, 
        cumulativamente, aos seguintes requisitos:"
        ),
        tags$li(
        "I - ser residente no Município do Recife, há pelo menos 02 (dois)
        anos;"),
        tags$li("II - ser inscrita no Cadastro Único, instituído pelo 
                Art. 6°-F da Lei Federal no 8.742, de 07 de dezembro de 1993;"),
        tags$li("III - não ter sido contemplada, em caráter definitivo, 
                com programas habitacionais de interesse social;"),
        tags$li("IV - possuir pelo menos um membro da família, em idade adulta,
                nos termos da legislação civil"),
        tags$li("§ 1° É vedada a percepção simultânea, pelo 
                beneficiário locatário ou por outro integrante de seu núcleo 
                familiar, do subsídio do Programa Continuar  Bom de Morar com o
                benefício de auxílio-moradia, Lei Ordinária 18.967 2022 de 
                Recife PE de aluguel social ou com outro benefício com
                mesmo fundamento destes, custeado por qualquer ente federativo."),
        tags$li("§ 2° Excepcionalmente, poderão ser isentadas do atendimento 
        ao inciso III deste artigo, as  mulheres em situação de violência 
        doméstica, mediante comprovação por meio de boletim de ocorrência e 
        denúncia de violência contra a mulher registrada junto aos órgãos 
                competentes."),
        tags$br(),
        
        tags$p("Art. 3° O ingresso da família ou indivíduo no Programa Bom de
        Morar dependerá da existência de imóvel compatível com a necessidade e 
        renda familiar e que esteja cadastrado no banco de imóveis do programa."),
        tags$br(),
        
        tags$p("Art. 4° As famílias de baixa renda a serem contempladas pelo 
        Programa Bom de Morar deverão formalizar sua concordância aos termos 
        do Programa por meio de sua anuência em Termo de Adesão a ser 
               disponibilizado pelo órgão gestor do Programa."),
        tags$li("Parágrafo único. A assinatura do Termo de Adesão 
               pelo futuro beneficiário locatário fica condicionada à 
               comprovação da atualização dos dados no Cadastro Único nos 
               últimos 24 (vintee quatro) meses."),
        tags$br(),
        tags$p("Art. 5° É permitida a migração entre o Programa Bom de Morar e
        outros programas habitacionais de qualquer ente da Federação, com base
        em análise do perfil socioeconômico da família.")
        )
}

#' Informações Server
#'
#' @import shiny
#'
informacoesServer = function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            
        }
    )
}



informacoesApp = function() {
    ui = informacoesUI("id1")
    server = function(input, output, session) {
        informacoesServer("id1")
    }
    shinyApp(ui, server)
}

#informacoesApp()



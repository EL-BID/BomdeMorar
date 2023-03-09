#' Função para buscar informação de indivíduos no CadÚnico
#' 
#' @description 
#' Essa função irá buscar as informações socio demográficas do CPF na base do CadÚnico.
#' 
#' @param cpf caractere com número do CPF do indivíduo. Esse número possui 11 digitos.
#' @param file_path endereço do arquivo da base do CadÚnico. Default para "data/cadunico/.
#' 
#' @importFrom dplyr %>%
#' 
#' @export

getInfoCAD_CPF = function(cpf, 
                          file_path = "data/cadunico/") { 
    
    # Filtering for CPF:
    arrow::open_dataset(sources = file_path) %>% 
    dplyr::filter(p.num_cpf_pessoa == cpf) %>% 
    dplyr::select(p.ref_cad,                  # data de referencia do CAD
                  d.dat_atual_fam,            # data da ultima atualizacao da família
                  d.cod_familiar_fam,         # codigo da família
                  p.cod_parentesco_rf_pessoa, # Relacao de parentesco com o responsavel familiar
                  p.nom_pessoa,               # nome completo do solicitante
                  p.dta_nasc_pessoa,          # data de nascimento do solicitante
                  p.num_cpf_pessoa,           # numero do cpf do solicitante
                  p.num_nis_pessoa_atual,     # numero nis do solicitante
                  d.nom_tip_logradouro_fam,   # rua, avenida, etc
                  d.nom_logradouro_fam,       # nome da rua/avenida
                  d.num_logradouro_fam,       # numero da casa
                  d.nom_localidade_fam,       # nome do bairro
                  d.vlr_renda_total_fam,      # renda total da família
                  d.vlr_renda_media_fam) %>% 
        dplyr::collect()
}

#' Função para buscar informação de indivíduos no CadÚnico
#' 
#' @description 
#' Essa função irá buscar as informações socio demográficas do NIS na base do CadÚnico.
#' 
#' @param nis caractere com número do CPF do indivíduo. Esse número possui 11 digitos.
#' @param file_path endereço do arquivo da base do CadÚnico. Default para "data/cadunico/.
#' 
#' @importFrom dplyr %>%
#' 
#' @export

getInfoCAD_NIS = function(nis, 
                          file_path = "data/cadunico/") { 
    # open connection to parquet file:
    arrow::open_dataset(sources = file_path) %>% 
    # Filtering for NIS:
    dplyr::filter(p.num_nis_pessoa_atual == nis) %>% 
    dplyr::select(
        p.ref_cad,                  # data de referencia do CAD
        d.dat_atual_fam,            # data da ultima atualizacao da família
        d.cod_familiar_fam,         # codigo da família
        p.cod_parentesco_rf_pessoa, # Relacao de parentesco com o responsavel familiar
        p.nom_pessoa,               # nome completo do solicitante
        p.dta_nasc_pessoa,          # data de nascimento do solicitante
        p.num_cpf_pessoa,           # numero do cpf do solicitante
        p.num_nis_pessoa_atual,     # numero nis do solicitante
        d.nom_tip_logradouro_fam,   # rua, avenida, etc
        d.nom_logradouro_fam,       # nome da rua/avenida
        d.num_logradouro_fam,       # numero da casa
        d.nom_localidade_fam,       # nome do bairro
        d.vlr_renda_total_fam,      # renda total da família
        d.vlr_renda_media_fam
    ) %>% 
        dlpyr::collect()
}

#' Função para buscar informações de famílias no CadÚnico
#' 
#' @description 
#' Essa função irá buscar as informações de todos membros da família na base 
#' do CadÚnico a partir de seu código de identificação.
#' 
#' @param cod_familiar caractere com o código de identificação da família no CadÚnico
#' @param file_path endereço do arquivo da base do CadÚnico. Default para "data/cadunico/.
#' 
#' @importFrom dplyr %>%
#' 
#' @export

getFamilyCAD <- function(cod_familiar,
                         file_path = "data/cadunico"){
    
    # open connection to parquet file:
    arrow::open_dataset(sources = file_path) %>% 
    # Filtering for cod_familiar:
    dplyr::filter(d.cod_familiar_fam == cod_familiar) %>% 
    # selecting columns:
    dplyr::select( 
        p.ref_cad,                    # data de referencia do CAD
        d.dat_atual_fam,              # data da ultima atualizacao da familia
        d.cod_familiar_fam,           # codigo da família
        p.cod_parentesco_rf_pessoa,   # Relaçao de parentesco com o RF
        p.num_cpf_pessoa,             # número de CPF
        p.num_nis_pessoa_atual,       # número do NIS
        p.nom_pessoa,                 # nome da pessoa
        p.dta_nasc_pessoa,            # data de nascimento
        p.cod_sexo_pessoa,            # sexo da pessoa
        d.nom_tip_logradouro_fam,     # rua, avenida, etc
        d.nom_logradouro_fam,         # nome da rua/avenida
        d.num_logradouro_fam,         # numero da casa
        d.nom_localidade_fam,         # nome do bairro
        d.num_cep_logradouro_fam,     # CEP
        d.vlr_renda_total_fam,        # renda total da familia
        d.vlr_renda_media_fam,        # renda média da família
        d.val_desp_aluguel_fam,       # valor de despesas com aluguel
        d.cod_material_domic_fam,     # material predominante nas paredes externas do domicílio
        d.qtd_familias_domic_fam      # quantidade de familias no domicilio
    ) %>% 
    dplyr::collect()
}

#' Função para checar inscrição no CadUnico
#' 
#' @description 
#' Essa função irá analisar se o CPF informado está inscrito no CadUnico.
#' 
#' @param cpf caracter com o número do CPF.
#' @param file_path endereço do arquivo da base do CadÚnico. Default para "data/cadunico/.
#' 
#' @importFrom dplyr %>%
#' 
#' @export
 
checkCAD_CPF = function(cpf,
                        file_path = "data/cadunico/") {

    
    flag = arrow::open_dataset(sources = file_path) %>% 
        # Filtering for CPF:
        dplyr::filter(p.num_cpf_pessoa == cpf) %>% 
        # selecting columns:
        dplyr::select(
            d.cod_familiar_fam,    # codigo da família
            p.nom_pessoa,          # nome completo do solicitante
            p.dta_nasc_pessoa,     # data de nascimento do solicitante
            p.num_cpf_pessoa,      # numero do cpf
            p.num_nis_pessoa_atual # numero nis do solicitante
        ) %>%
        dplyr::compute() |> 
        # count number of rows:
        nrow()
    # check if there is return:
    flag > 0
}

#' Função para checar inscrição no CadUnico
#' 
#' @description 
#' Essa função irá analisar se o NIS informado está inscrito no CadUnico.
#' 
#' @param nis caracter com o número do NIS.
#' @param file_path endereço do arquivo da base do CadÚnico. Default para "data/cadunico/.
#' 
#' @importFrom dplyr %>%
#' 
#' @export

checkCAD_NIS = function(nis,
                        file_path = "data/cadunico/") {
    
    flag = arrow::open_dataset(sources = file_path) %>% 
        # Filtering for NIS:
        dplyr::filter(p.num_nis_pessoa_atual == nis) %>% 
        # selecting columns:
        dplyr::select(
            d.cod_familiar_fam, # codigo da família
            p.nom_pessoa, # nome completo do solicitante
            p.dta_nasc_pessoa, # data de nascimento do solicitante
            p.num_cpf_pessoa,   # numero do cpf
            p.num_nis_pessoa_atual # numero nis do solicitante
        ) %>%
        dplyr::compute() |> 
        nrow()
    flag > 0
}

#' Checar adulto na família
#'
#' @description 
#' Essa função irá checar se há algum membro na família com idade acima de 
#' 18 anos a partir das informações do CadÚnico.
#' 
#' @param family base de dados com informações de todos os membros da família.
#' 
#' @importFrom lubridate %--%
#' 
#' @export

checkAnyAdultFamily <- function(family) {
    # calculando idade de cada membro:
    familyInfo = family |> 
        dplyr::mutate(idade = trunc((p.dta_nasc_pessoa %--% lubridate::today())/ lubridate::years(1)))
    # checando se existe alguém com idade acima de 18 anos:
    any(familyInfo$idade > 18)
}

#' Checar recebimento de auxílio Moradia
#' 
#' @description 
#' Essa função irá checar se há algum membro da família que recebe benefício do 
#' Programa Auxílio Moradia.
#' 
#' @details 
#' Essa função realizará uma operação de `join` junto a base do programa Auxílio
#' Moradia utilizando a informação do CPF do indivíduo.
#' 
#' @param family base de dados com informações de todos os membros da família.
#' @param file_path caminho para o arquivo da base de auxilio moradia. 
#' 
#' @export

checkAuxilioMoradia_NIS <- function(family, 
                                file_path = "data/auxilioMoradia/") {
    
    # pegando os indivíduos ativos do programa Auxilio Moradia
    flag = arrow::open_dataset(file_path) |> 
        dplyr::filter(`ATIVO` == "sim") |> 
        # merge by NIS:
        dplyr::inner_join(
            family, 
            by = c("NIS" = "p.num_nis_pessoa_atual")
        ) |> 
        dplyr::compute() |> 
        nrow()
    
    flag > 0
}

#' Checar recebimento de auxílio Moradia
#' 
#' @description 
#' Essa função irá checar se há algum membro da família que recebe benefício do 
#' Programa Auxílio Moradia.
#' 
#' @details 
#' Essa função realizará uma operação de `join` junto a base do programa Auxílio
#' Moradia utilizando a informação do NIS do indivíduo.
#' 
#' @param family base de dados com informações de todos os membros da família.
#' @param file_path caminho para o arquivo da base de auxilio moradia. 
#' 
#' @export

checkAuxilioMoradia_CPF <- function(family, 
                                    file_path = "data/auxilioMoradia/") {
    
    # pegando os indivíduos ativos do programa Auxilio Moradia
    flag = arrow::open_dataset(file_path) |> 
        dplyr::filter(`ATIVO` == "sim") |> 
        # merge by CPF:
        dplyr::inner_join(
            family, 
            by = c("CPF" = "p.num_cpf_pessoa")
        ) |> 
        dplyr::compute() |> 
        nrow()
    
    flag > 0
}


#' Checar recebimento de auxílio Moradia
#' 
#' @description 
#' Essa função irá checar se há algum membro da família que recebe benefício do 
#' Programa Auxílio Moradia.
#' 
#' @details 
#' Essa função realizará uma operação de `join` junto a base do programa Auxílio
#' Moradia utilizando a informação de `nome` e `data de nascimento` do indivíduo.
#' 
#' @param family base de dados com informações de todos os membros da família.
#' @param file_path caminho para o arquivo da base de auxilio moradia. 
#' 
#' @export

checkAuxilioMoradia_NomeDtaNasc <- function(family, 
                                            file_path = "data/auxilioMoradia/") {
    
    # pegando os indivíduos ativos do programa Auxilio Moradia
    flag = arrow::open_dataset(file_path) |> 
        dplyr::filter(`ATIVO` == "sim") |> 
        # merge by Nome e Data de Nascimento:
        dplyr::inner_join(
            family, 
            by = c("NOME" = "p.nom_pessoa",
                   "DATA NASCIMENTO" = "p.dta_nasc_pessoa")
        ) |> 
        dplyr::compute() |> 
        nrow()
    
    flag > 0
}


#' Checar Recebimento de Auxílio Acolhida
#' 
#' @description 
#' Essa função irá checar se há algum membro da família que recebe benefício do 
#' Programa Auxílio Acolhida (Antigo Aluguel Social):
#' 
#' @details 
#' Essa função realizará uma operação de `join` junto a base do programa Auxílio
#' Acolhida utilizando a informação do número `NIS` do indivíduo.
#' 
#' @param family base de dados com informações de todos os membros da família.
#' @param file_path caminho para o arquivo da base de auxilio moradia. 
#' 
#' @export

checkAuxilioAcolhida_NIS <- function(family, 
                                     file_path = "data/auxilioAcolhida/") {
    
    flag = arrow::open_dataset(file_path) |> 
        dplyr::filter(`SITUACAO DO BENEFICIO` == "ATIVO") |> 
        dplyr::select(`USUARIO`, `DATA DE NASC.`, `CPF`, `NIS`) |> 
        # merge by NIS:
        dplyr::inner_join(
            family, 
            by = c("NIS" = "p.num_nis_pessoa_atual")
        ) |> 
        dplyr::compute() |> 
        nrow()
    
    flag > 0
}

#' Checar Recebimento de Auxílio Acolhida
#' 
#' @description 
#' Essa função irá checar se há algum membro da família que recebe benefício do 
#' Programa Auxílio Acolhida (Antigo Aluguel Social):
#' 
#' @details 
#' Essa função realizará uma operação de `join` junto a base do programa Auxílio
#' Acolhida utilizando a informação do número `CPF` do indivíduo.
#' 
#' @param family base de dados com informações de todos os membros da família.
#' @param file_path caminho para o arquivo da base de auxilio moradia. 
#' 
#' @export

checkAuxilioAcolhida_CPF <- function(family, 
                                 file_path = "data/auxilioAcolhida/") {
    
    flag = arrow::open_dataset(file_path) |> 
        dplyr::filter(`SITUACAO DO BENEFICIO` == "ATIVO") |> 
        dplyr::select(`USUARIO`, `DATA DE NASC.`, `CPF`, `NIS`) |> 
        # merge by CPF:
        dplyr::inner_join(
            family, 
            by = c("CPF" = "p.num_cpf_pessoa")
        ) |> 
        dplyr::compute() |> 
        nrow()
    
    flag > 0
}

#' Checar Recebimento de Auxílio Acolhida
#' 
#' @description 
#' Essa função irá checar se há algum membro da família que recebe benefício do 
#' Programa Auxílio Acolhida (Antigo Aluguel Social):
#' 
#' @details 
#' Essa função realizará uma operação de `join` junto a base do programa Auxílio
#' Acolhida utilizando a informação do `Nome` e `Data de Nascimento` do indivíduo.
#' 
#' @param family base de dados com informações de todos os membros da família.
#' @param file_path caminho para o arquivo da base de auxilio moradia. 
#' 
#' @export

checkAuxilioAcolhida_NomeDtaNasc <- function(family, 
                                             file_path = "data/auxilioAcolhida/") {
    
    flag = arrow::open_dataset(file_path) |> 
        dplyr::filter(`SITUACAO DO BENEFICIO` == "ATIVO") |> 
        dplyr::select(`USUARIO`, `DATA DE NASC.`, `CPF`, `NIS`) |> 
        # Merge by Nome e data de nascimento
        dplyr::inner_join(
            family, 
            by = c("USUARIO" = "p.nom_pessoa",
                   "DATA DE NASC." = "p.dta_nasc_pessoa")
        ) |> 
        dplyr::compute() |> 
        nrow()
    
    flag > 0
}

#' Checar Atualização de Cadastro no CadÚnico
#' 
#' @description 
#' Função para checar se o cadastro foi atualizado nos últimos 24 meses:
#' 
#' @param dtaAtualizacaoCadastral data da última atualização cadastral
#' @param dtaRefCad data de referência do CAD
#' @param updated_within_months número de meses para checar atualização.
#' Default para 24 meses.
#' 
#' @importFrom lubridate %m-%
#' @export

checkCadastroAtualizadoCAD <- function(dtaAtualizacaoCadastral, dtaRefCad, updated_within_months = 24) {
    flag = dtaAtualizacaoCadastral >= (dtaRefCad %m-% months(updated_within_months) )
    flag 
}

#' Checar Elegibilidade do indivíduo.
#' 
#' @description 
#' Função para checar elegibilidade ao programa utilizando o CPF:
#' 
#' @param cpf número de cpf do indivíduos
#' 
#' @export

checkElegibility_CPF = function(cpf) {

    # I. ser residente do município do Recife, há pelo menos 02 anos:
    condition1 = T
    
    if( condition1 ) {
        
        # II. ser inscrito no cadúnico
        condition2 = checkCAD_CPF(cpf)
        condition2
        
        if( condition2 ) {
            
            # getting informações do indivíduos
            infoCPF = getInfoCAD_CPF(cpf)
            infoCPF
            
            # getting info da família:
            infoFamily = getFamilyCAD(infoCPF$d.cod_familiar_fam)
            infoFamily
            
            # III. Não ter sido contemplada, em caráter definitivo, com programas 
            # habitacionais de interesse social;
            # checkProgramaHabitacional()
            condition3 = T
    
            if ( condition3 ) {
                # IV. possuir pelo menos um membro da família, em idade adulta,
                # nos termos da legislação civil brasileira:
                condition4 = checkAnyAdultFamily(infoFamily)
                condition4
                
                if( condition4 ) {
                    # V. É vedada a percepção simultânea, pelo beneficiário 
                    # locatário ou por outro integrante de seu núcleo familiar, do 
                    # subsídio do Programa Bom de Morar com o benefício de 
                    # auxílio-moradia, de aluguel social ou com outro benefício 
                    # com mesmo fundamento destes, custeado por qualquer ente 
                    # federativo.
                    
                    # checando se auxilio moradia:
                    recebeAuxilioMoradia = ( checkAuxilioMoradia_CPF(family = infoFamily) || checkAuxilioMoradia_NomeDtaNasc(family = infoFamily) )
                    
                    # checando se recebe auxilio acolhida:
                    recebeAuxilioAcolhida = (  checkAuxilioAcolhida_CPF(family = infoFamily) || checkAuxilioAcolhida_NomeDtaNasc(family = infoFamily) )
                    
                    condition5 = ! (
                        recebeAuxilioAcolhida || recebeAuxilioMoradia
                        # # check por CPF 
                        # ( checkAuxilioMoradia_CPF(family = infoFamily) || checkAuxilioAcolhida_CPF(family = infoFamily) ) 
                        # || 
                        # # check por Nome e data de nascimento
                        # ( checkAuxilioMoradia_NomeDtaNasc(family = infoFamily) || checkAuxilioAcolhida_NomeDtaNasc(family = infoFamily) )
                        
                    )
                    condition5
                    
                    if( condition5 ) {
                        # VI. A assinatura do Termo de Adesão pelo futuro
                        # beneficiário locatário fica condicionada à comprovação
                        # da atualização dos dados no Cadastro Único nos últimos
                        # 24 (vinte e quatro) meses.
                        condition6 = checkCadastroAtualizadoCAD(
                            dtaAtualizacaoCadastral = infoCPF$d.dat_atual_fam, 
                            dtaRefCad = infoCPF$p.ref_cad
                        )
                        condition6
                        
                        if ( condition6 ) {
                            
                            out = list(
                                resultado = "elegivel",
                                infoFamily = infoFamily
                            )
                            
                        } else {
                            out = list(
                                resultado = "inelegivel",
                                motivo = "",
                                dispositivo = "Art 4°, Parágrafo único da Lei Municipal N° 
                                18.967: A assinatura do Termo de Adesão pelo futuro 
                                beneficiário locatário fica condicionada à 
                                comprovação da atualização dos dados no Cadastro 
                                Único nos últimos 24 (vinte e quatro) meses."
                            )
                        }
                    } else {
                        out = list(
                            resultado = "inelegivel",
                            motivo = dplyr::case_when(
                                recebeAuxilioAcolhida ~ "Beneficiário do programa Auxílio Acolhida",
                                recebeAuxilioMoradia  ~ "Beneficiário do programa Auxílio Moradia"
                            ),
                            dispositivo = "Art 2°, Parágrafo 1° da Lei Municipal N° 
                            18.967: É vedada a percepção simultânea, pelo 
                            beneficiário locatário ou por outro integrante de
                            seu núcleo familiar, do subsídio do Programa 
                            Bom de Morar com o benefício de auxílio-moradia, de 
                            aluguel social ou com outro benefício com mesmo 
                            fundamento destes, custeado por qualquer ente 
                            federativo."
                        )
                    }
                } else {
                    out = list(
                        resultado = "inelegivel",
                        motivo = "",
                        dispositivo = "Art. 2°, Inciso IV da Lei Municipal N° 18.967: 
                        possuir pelo menos um membro da família, em idade adulta, 
                        nos termos da legislação civil brasileira;"
                    )
                }
                
            } else {
                out = list(
                    resultado = "inelegivel",
                    motivo = "",
                    dispositivo = "Art. 2°, Inciso III da Lei Municipal N° 18.967: não ter 
                    sido contemplada, em caráter definitivo, com programas 
                    habitacionais de interesse social;"
                )
            }
            
        } else {
            out = list(
                resultado = "inelegivel",
                motivo = "",
                dispositivo = "Art. 2°, Inciso II da Lei Municipal N° 18.967: ser inscrita no 
                Cadastro Único, instituído pelo Art. 6°-F da Lei Federal N° 8.742, 
                de 07 de dezembro de 1993;"
            )
        }
        
    } else {
        out = list(
            resultado = "inelegivel",
            motivo = "",
            dispositivo = "Art. 2°, Inciso I da Lei Municipal N° 18.967: ser residente no 
            Municipio do Recife, ha pelo menos 02 (dois) anos;"
        )
    }

    return( out )
}


#' Checar Elegibilidade do indivíduo.
#' 
#' @description 
#' Função para checar elegibilidade ao programa utilizando o `NIS`.
#' 
#' @param nis número de `NIS` do indivíduo
#' 
#' @export

checkElegibility_NIS = function(nis) {

    # I. ser residente do município do Recife, há pelo menos 02 anos:
    condition1 = T
    
    if( condition1 ) {
        
        # II. ser inscrito no cadúnico
        condition2 = checkCAD_NIS(nis)
        condition2
        
        if( condition2 ) {
            
            # getting informações do indivíduos
            infoCPF = getInfoCAD_NIS(nis)
            infoCPF
            
            # getting info da família:
            infoFamily = getFamilyCAD(infoCPF$d.cod_familiar_fam)
            infoFamily
            
            # III. Não ter sido contemplada, em caráter definitivo, com programas 
            # habitacionais de interesse social;
            # checkProgramaHabitacional()
            condition3 = T
    
            if ( condition3 ) {
                # IV. possuir pelo menos um membro da família, em idade adulta,
                # nos termos da legislação civil brasileira:
                condition4 = checkAnyAdultFamily(infoFamily)
                condition4
                
                if( condition4 ) {
                    # V. É vedada a percepção simultânea, pelo beneficiário 
                    # locatário ou por outro integrante de seu núcleo familiar, do 
                    # subsídio do Programa Bom de Morar com o benefício de 
                    # auxílio-moradia, de aluguel social ou com outro benefício 
                    # com mesmo fundamento destes, custeado por qualquer ente 
                    # federativo.
                    condition5 = ! (
                        # auxilio Moradia e auxilio acolhida por CPF:
                        ( checkAuxilioMoradia_CPF(family = infoFamily) || checkAuxilioAcolhida_CPF(family = infoFamily) ) 
                        || 
                        # auxilio Moradia e auxilio Acolhida por Nome e Dta de Nascimento:
                        ( checkAuxilioMoradia_NomeDtaNasc(family = infoFamily) || checkAuxilioAcolhida_NomeDtaNasc(family = infoFamily) )
                    )
                    condition5
                    
                    if( condition5 ) {
                        # VI. A assinatura do Termo de Adesão pelo futuro
                        # beneficiário locatário fica condicionada à comprovação
                        # da atualização dos dados no Cadastro Único nos últimos
                        # 24 (vinte e quatro) meses.
                        condition6 = checkCadastroAtualizado(
                            dtaAtualizacaoCadastral = infoCPF$d.dat_atual_fam, 
                            dtaRefCad = infoCPF$p.ref_cad
                        )
                        condition6
                        
                        if ( condition6 ) {
                            
                            out = list(
                                resultado = "elegivel",
                                motivo = "",
                                dispositivo = ""
                            )
                        } else {
                            out = list(
                                resultado = "inelegivel",
                                motivo = "violacao art. 4, paragrafo unico",
                                dispositivo = "art 4, paragrafo único da lei municipal no 
                                18967: A assinatura do Termo de Adesão pelo futuro 
                                beneficiário locatário fica condicionada à 
                                comprovação da atualização dos dados no Cadastro 
                                Único nos últimos 24 (vinte e quatro) meses."
                            )
                        }
                    } else {
                        out = list(
                            resultado = "inelegivel",
                            motivo = "violacao art. 2, paragrafo 1",
                            dispositivo = "art 2, paragrafo 1 da lei municipal no 
                            18967: É vedada a percepção simultânea, pelo 
                            beneficiário locatário ou por outro integrante de
                            seu núcleo familiar, do subsídio do Programa 
                            Bom de Morar com o benefício de auxílio-moradia, de 
                            aluguel social ou com outro benefício com mesmo 
                            fundamento destes, custeado por qualquer ente 
                            federativo."
                        )
                    }
                } else {
                    out = list(
                        resultado = "inelegivel",
                        motivo = "violacao art. 2, iv",
                        dispositivo = "art 2, iii da lei municipal no 18967: 
                        possuir pelo menos um membro da família, em idade adulta, 
                        nos termos da legislação civil brasileira;"
                    )
                }
                
            } else {
                out = list(
                    resultado = "inelegivel",
                    motivo = "violacao art. 2, iii",
                    dispositivo = "art 2, iii da lei municipal no 18967: não ter 
                    sido contemplada, em caráter definitivo, com programas 
                    habitacionais de interesse social;"
                )
            }
            
        } else {
            out = list(
                resultado = "inelegivel",
                motivo = "violacao art. 2, ii",
                dispositivo = "art 2, ii da lei municipal no 18967: ser inscrita no 
                Cadastro Único, instituído pelo Art. 6-F da Lei Federal no 8.742, 
                de 07 de dezembro de 1993;"
            )
        }
        
    } else {
        out = list(
            resultado = "inelegivel",
            motivo = "violacao art. 2, i",
            dispositivo = "art 2, i da lei municipal no 18967: ser residente no 
            Municipio do Recife, ha pelo menos 02 (dois) anos;"
        )
    }

    return( out )
}

#' Função para Calcular Subsídio.
#' 
#' @description 
#' Essa função para calcular a modalidade do programa e valor do subsídio.
#' 
#' @param infoFamily dataframe com as informações da família extraídas do CadUnico
#' @param SM Valor do salário mínimo
#' @param SUBSIDIO_MIN valor do subsídio mínimo
#' @param SUBSIDIO_MAX valor do subsídio máximo
#' @param ALGUEL_MAX valor do aluguel máximo

playProgramaBomDeMorar = function(infoFamily, SM, SUBSIDIO_MIN, SUBSIDIO_MAX, ALUGUEL_MAX) {
    
    # PARAMETROS:
    SUBSIDIO_MAX  = 600
    SUBSIDIO_MIN  = 50
    ALUGUEL_MAX   = 1000
    SM            = 1212
    
    # qual modalidade do programa bom de Morar?
    
    # check mora em domicilio rustico (domicilio sem parede de alvenaria ou madeira aparelhada):
    infoFamily = infoFamily |> 
        dplyr::mutate(domicilio_rustico = !(d.cod_material_domic_fam %in% c(1, 2)))
    infoFamily
    
    # check area de intervenção municipal:
    infoFamily = infoFamily |> 
        dplyr::mutate(area_intervencao = TRUE)
    
    # check ônus excessivo:
    infoFamily = infoFamily |> 
        dplyr::mutate(onus_excessivo = d.val_desp_aluguel_fam > 0.3*d.vlr_renda_total_fam)
    
    # check coabitação involuntária:
    infoFamily = infoFamily |> 
        dplyr::mutate(coabitacao_involuntaria = d.qtd_familias_domic_fam > 1)
    infoFamily
    
    # determinando modalidade do programa:
    infoFamily = infoFamily |> 
        dplyr::mutate(modalidade = dplyr::case_when(
            any(domicilio_rustico) ~ "Bom de Morar I", 
            any(onus_excessivo) | any(coabitacao_involuntaria) ~ "Bom de Morar II",
            TRUE ~ "Sem modalidade"
            )
        )
    
    # Cálculo do comprometimento de Renda:
    infoFamily = infoFamily |> 
        dplyr::mutate(CR = dplyr::case_when(
            d.vlr_renda_total_fam <= 1*SM                               ~ 0.15*d.vlr_renda_total_fam,
            d.vlr_renda_total_fam > 1*SM & d.vlr_renda_total_fam < 3*SM ~ (.1 + (d.vlr_renda_total_fam/SM)*0.05)*d.vlr_renda_total_fam,
            d.vlr_renda_total_fam == 3*SM                               ~ 0.25*d.vlr_renda_total_fam,
            TRUE                                                        ~ (.1 + (3)*0.05)*d.vlr_renda_total_fam
            )
        )
    infoFamily = infoFamily |> 
        dplyr::mutate(CR = round(CR, 2))
    
    # Cálculo do subsidio max:
    infoFamily = infoFamily |> 
        dplyr::mutate(
            subsidio_max = ifelse(CR >= ALUGUEL_MAX, 0, max(SUBSIDIO_MIN, min(SUBSIDIO_MAX, ALUGUEL_MAX - CR)))
            )
    
    # Cálculo do aluguel max:
    infoFamily = infoFamily |> 
        dplyr::mutate(aluguel_max = min(ALUGUEL_MAX, CR + subsidio_max))
    
    # Cálculo do aluguel min:
    infoFamily = infoFamily |> 
        dplyr::mutate(aluguel_min = min(ALUGUEL_MAX, CR + SUBSIDIO_MIN))
    
    infoFamily
}


#' Lista de Imóveis Disponíveis
#' 
#' @description 
#'  Função para selecionar os imóveis possíveis, dado o nível de comprometimento
#'  de renda da família.
#'  
#'  @param cr comprometimento de renda
#'  @param aluguel_up valor máximo de aluguel
#'  @param aluguel_lw valor mínumo de aluguel
#'  @param file_path path  do banco de dados de imóveis
#'  
#'  @export

listaImoveisDisp <- function(cr,
                             aluguel_up,#comp_renda_fam, 
                             aluguel_lw,#subsidio_max,
                             #subsidio_min,
                             file_path = "data/imoveis/") {
    # Valor máximo de locação:
    #max_vlr_aluguel = comp_renda_fam + subsidio_max
    # querying parquet dataset:
    imoveis_psb = arrow::open_dataset(file_path) %>% 
        dplyr::filter(preco_aluguel >= aluguel_lw & preco_aluguel <= aluguel_up) %>% 
        dplyr::collect()
    # calculando:
    imoveis_psb %>% 
        dplyr::mutate(
            comp_renda_fam = cr,
            #subsidio_max = subsidio_max,
            #subsidio_min = subsidio_min,
            subsidio_efetivo = preco_aluguel - comp_renda_fam
        ) %>% 
        #group_by(apt_id) %>% 
        #mutate(subsidio_efetivo = max(subsidio_min, subsidio_efetivo)) %>% 
        #ungroup() %>% 
        #mutate(
        #    subsidio_max = NULL,
        #    subsidio_min = NULL
        #) %>% 
        dplyr::arrange(dplyr::desc(subsidio_efetivo))
}


#' Plotar mapa com imóveis
#' 
#' @description 
#' Essa função irá plotr o mapa com os imóveis.
#' 
#' @param imoveis banco de dados com os imóveis
#' 
#' @export

plotMap <- function(imoveis) { 
    imoveis %>% 
        #head(10) %>% 
        dplyr::mutate(
            info = paste(
                sep="<br/>", 
                stringr::str_c("Aluguel: <b>R$ ", preco_aluguel, "</b>"),
                stringr::str_c("Sudsídio: <b>R$ ", subsidio_efetivo, "</b>"),
                endereco,
                stringr::str_c(andar, "° Andar"),
                stringr::str_c("Área: ", round(area_m2), " M2")
                )
        ) %>% 
        leaflet::leaflet(height=1000) %>% 
        leaflet::addTiles() %>% 
        leaflet::addMarkers(
            lng = ~lon, lat = ~lat, 
            popup = ~info,
            popupOptions = list(textsize="20px"),
            label = ~paste0("R$ ", preco_aluguel),
            labelOptions = list(noHide = T, 
                                textsize = "14px",
                                style = list(
                                    "font-weight" = "bold", # bold text
                                    padding = "5px" # padding around text
                                    )
                                )
            )
}


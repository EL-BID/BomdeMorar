# Função para filtrar solicitante do cadunico
getInfoCAD_CPF = function(cpf, 
                          file_path = "data/cadunico/") { 
    library(arrow)
    library(dplyr)
    # Filtering for CPF:
    open_dataset(sources = file_path) %>% 
    filter(p.num_cpf_pessoa == cpf) %>% 
    select(p.ref_cad,                  # data de referencia do CAD
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
    collect()
}

# Função para filtrar solicitante do cadunico
getInfoCAD_NIS = function(nis, 
                          file_path = "data/cadunico/") { 
    library(arrow)
    library(dplyr)
    # Filtering for CPF:
    open_dataset(sources = file_path) %>% 
    filter(p.num_nis_pessoa_atual == nis) %>% 
    select(p.ref_cad,                  # data de referencia do CAD
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
    collect()
}


# Função para retornar todos membros da família do solicitante 
# (incluindo o solicitante) usando o cadunico:
getFamilyCAD <- function(cod_familiar,
                         file_path = "data/cadunico"){
    library(arrow)
    library(dplyr)
    # Filtering for cod_familiar:
    open_dataset(sources = file_path) %>% 
    filter(d.cod_familiar_fam == cod_familiar) %>% 
    select( 
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
    collect()
}


# Função para checar se há o solicitante está inscrito no CadUnico:
checkCAD_CPF = function(cpf,
                        file_path = "data/cadunico/") {
    
    library(arrow)
    library(dplyr)
    # Filtering for CPF:
    flag = open_dataset(sources = file_path) %>% 
        filter(p.num_cpf_pessoa == cpf) %>% 
        select(
            d.cod_familiar_fam, # codigo da família
            p.nom_pessoa, # nome completo do solicitante
            p.dta_nasc_pessoa, # data de nascimento do solicitante
            p.num_cpf_pessoa,   # numero do cpf
            p.num_nis_pessoa_atual # numero nis do solicitante
        ) %>%
        compute() |> 
        nrow()
    flag > 0
}


checkCAD_NIS = function(nis,
                        file_path = "data/cadunico/") {
    library(arrow)
    library(dplyr)
    # Filtering for CPF:
    flag = open_dataset(sources = file_path) %>% 
        filter(p.num_nis_pessoa_atual == nis) %>% 
        select(
            d.cod_familiar_fam, # codigo da família
            p.nom_pessoa, # nome completo do solicitante
            p.dta_nasc_pessoa, # data de nascimento do solicitante
            p.num_cpf_pessoa,   # numero do cpf
            p.num_nis_pessoa_atual # numero nis do solicitante
        ) %>%
        compute() |> 
        nrow()
    flag > 0
}

# Função para checar se há membro com idade acima de 18 anos na família
# a partir das informações do CAD:
checkAnyAdultFamily <- function(family) {
    # calculando idade de cada membro:
    familyInfo = family |> 
        mutate(idade = trunc((p.dta_nasc_pessoa %--% today())/ years(1)))
    # checando se existe alguém com idade acima de 18 anos:
    any(familyInfo$idade > 18)
}


# Função para checar se há algum membro da família que recebe benefício do 
# Programa Auxílio Moradia:
checkAuxilioMoradia_NIS <- function(family, 
                                file_path = "data/auxilioMoradia/") {
    
    # pegando os indivíduos ativos do programa Auxilio Moradia
    flag = open_dataset(file_path) |> 
        filter(`ATIVO` == "sim") |> 
        # merge by NIS:
        inner_join(
            family, 
            by = c("NIS" = "p.num_nis_pessoa_atual")
        ) |> 
        compute() |> 
        nrow()
    
    flag > 0
}

# Função para checar se há algum membro da família que recebe benefício do 
# Programa Auxílio Moradia:
checkAuxilioMoradia_CPF <- function(family, 
                                    file_path = "data/auxilioMoradia/") {
    
    # pegando os indivíduos ativos do programa Auxilio Moradia
    flag = open_dataset(file_path) |> 
        filter(`ATIVO` == "sim") |> 
        # merge by CPF:
        inner_join(
            family, 
            by = c("CPF" = "p.num_cpf_pessoa")
        ) |> 
        compute() |> 
        nrow()
    
    flag > 0
}

# Função para checar se há algum membro da família que recebe benefício do 
# Programa Auxílio Moradia:
checkAuxilioMoradia_NomeDtaNasc <- function(family, 
                                            file_path = "data/auxilioMoradia/") {
    
    # pegando os indivíduos ativos do programa Auxilio Moradia
    flag = open_dataset(file_path) |> 
        filter(`ATIVO` == "sim") |> 
        # merge by Nome e Data de Nascimento:
        inner_join(
            family, 
            by = c("NOME" = "p.nom_pessoa",
                   "DATA NASCIMENTO" = "p.dta_nasc_pessoa")
        ) |> 
        compute() |> 
        nrow()
    
    flag > 0
}

# Função para checar se há algum membro da família que recebe benefício do 
# Programa Auxílio Acolhida (Antigo Aluguel Social):
checkAuxilioAcolhida_NIS <- function(family, 
                                     file_path = "data/auxilioAcolhida/") {
    
    flag = open_dataset(file_path) |> 
        filter(`SITUACAO DO BENEFICIO` == "ATIVO") |> 
        select(`USUARIO`, `DATA DE NASC.`, `CPF`, `NIS`) |> 
        # merge by NIS:
        inner_join(
            family, 
            by = c("NIS" = "p.num_nis_pessoa_atual")
        ) |> 
        compute() |> 
        nrow()
    
    flag > 0
}

# Função para checar se há algum membro da família que recebe benefício do 
# Programa Auxílio Acolhida (Antigo Aluguel Social):
checkAuxilioAcolhida_CPF <- function(family, 
                                 file_path = "data/auxilioAcolhida/") {
    
    flag = open_dataset(file_path) |> 
        filter(`SITUACAO DO BENEFICIO` == "ATIVO") |> 
        select(`USUARIO`, `DATA DE NASC.`, `CPF`, `NIS`) |> 
        # merge by CPF:
        inner_join(
            family, 
            by = c("CPF" = "p.num_cpf_pessoa")
        ) |> 
        compute() |> 
        nrow()
    
    flag > 0
}

# Função para checar se há algum membro da família que recebe benefício do 
# Programa Auxílio Acolhida (Antigo Aluguel Social):
checkAuxilioAcolhida_NomeDtaNasc <- function(family, 
                                             file_path = "data/auxilioAcolhida/") {
    
    flag = open_dataset(file_path) |> 
        filter(`SITUACAO DO BENEFICIO` == "ATIVO") |> 
        select(`USUARIO`, `DATA DE NASC.`, `CPF`, `NIS`) |> 
        # Merge by Nome e data de nascimento
        inner_join(
            family, 
            by = c("USUARIO" = "p.nom_pessoa",
                   "DATA DE NASC." = "p.dta_nasc_pessoa")
        ) |> 
        compute() |> 
        nrow()
    
    flag > 0
}

# Função para checar se o cadastro foi atualizado nos últimos 24 meses:
checkCadastroAtualizadoCAD <- function(dtaAtualizacaoCadastral, dtaRefCad, updated_within_months = 24) {
    library(lubridate)
    flag = dtaAtualizacaoCadastral >= (dtaRefCad %m-% months(updated_within_months) )
    flag 
}


# Função para checar elegibilidade ao programa utilizando o CPF:
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
                            motivo = case_when(
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


# Função para checar elegibilidade ao programa utilizando o NIS:
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


playProgramaBomDeMorar = function(infoFamily, SM, SUBSIDIO_MIN, SUBSIDIO_MAX, ALUGUEL_MAX) {
    
    # PARAMETROS:
    SUBSIDIO_MAX  = 600
    SUBSIDIO_MIN  = 50
    ALUGUEL_MAX   = 1000
    SM            = 1212
    
    # qual modalidade do programa bom de Morar?
    
    # check mora em domicilio rustico (domicilio sem parede de alvenaria ou madeira aparelhada):
    infoFamily = infoFamily |> mutate(domicilio_rustico = !(d.cod_material_domic_fam %in% c(1, 2)))
    infoFamily
    
    # check area de intervenção municipal:
    infoFamily = infoFamily |> mutate(area_intervencao = TRUE)
    
    # check ônus excessivo:
    infoFamily = infoFamily |> mutate(onus_excessivo = d.val_desp_aluguel_fam > 0.3*d.vlr_renda_total_fam)
    
    # check coabitação involuntária:
    infoFamily = infoFamily |> mutate(coabitacao_involuntaria = d.qtd_familias_domic_fam > 1)
    infoFamily
    
    # determinando modalidade do programa:
    infoFamily = infoFamily |> 
        mutate(modalidade = case_when(
            any(domicilio_rustico) ~ "Bom de Morar I", 
            any(onus_excessivo) | any(coabitacao_involuntaria) ~ "Bom de Morar II",
            TRUE ~ "Sem modalidade"
            )
        )
    
    # Cálculo do comprometimento de Renda:
    infoFamily = infoFamily |> 
        mutate(CR = case_when(
            d.vlr_renda_total_fam <= 1*SM                               ~ 0.15*d.vlr_renda_total_fam,
            d.vlr_renda_total_fam > 1*SM & d.vlr_renda_total_fam < 3*SM ~ (.1 + (d.vlr_renda_total_fam/SM)*0.05)*d.vlr_renda_total_fam,
            d.vlr_renda_total_fam == 3*SM                               ~ 0.25*d.vlr_renda_total_fam,
            TRUE                                                        ~ (.1 + (3)*0.05)*d.vlr_renda_total_fam
            )
        )
    infoFamily = infoFamily |> mutate(CR = round(CR, 2))
    
    # Cálculo do subsidio max:
    infoFamily = infoFamily |> 
        mutate(
            subsidio_max = ifelse(CR >= ALUGUEL_MAX, 0, max(SUBSIDIO_MIN, min(SUBSIDIO_MAX, ALUGUEL_MAX - CR)))
            )
    
    # Cálculo do aluguel max:
    infoFamily = infoFamily |> 
        mutate(aluguel_max = min(ALUGUEL_MAX, CR + subsidio_max))
    
    # Cálculo do aluguel min:
    infoFamily = infoFamily |> 
        mutate(aluguel_min = min(ALUGUEL_MAX, CR + SUBSIDIO_MIN))
    
    infoFamily
}



rendaTotalFam_SM <- function(d.vlr_renda_total_fam, 
                             SM) {
    round(d.vlr_renda_total_fam/SM, 2)
}

rendaMediaFam_SM <- function(d.vlr_renda_media_fam, 
                             SM) {
    round(d.vlr_renda_media_fam/SM, 2)
}


    
# Função para calcular elegibilidade no programa:
elegibilidadeFam <- function(vlr_renda_total_fam_SM, 
                             vlr_renda_media_fam_SM) {
    # CONDIÇÃO 1: Renda total da família menor ou igual a 3 SM?
    cond1 = vlr_renda_total_fam_SM <= 3
    # CONDIÇÃO 2: Renda total da família maior do que que 3 e menor ou igual a 5?
    cond2 = vlr_renda_total_fam_SM > 3 & vlr_renda_total_fam_SM <= 5
    # CONDIÇÃO 3: Renda média da família menor ou igual a 1 SM?
    cond3 = vlr_renda_media_fam_SM <= 1
    # família elegível?
    res = fcase(
        # Se Renda total da família menor ou igual a 3 SM, elegível; 
        cond1, TRUE, 
        # Se Renda total da família > 3 e <= 5 SM e a renda per capita nao superar 1 SM, elegível;
        cond1 & cond2 & cond3, TRUE, 
        default = FALSE)
    res
}

# Função para calcular o valor do compromentimento máximo da renda familiar
# em gasto de aluguel:
compMaxRendaFam <- function(vlr_renda_total_fam_SM,
                            SM) {
    # fracao da renda total da família em termos de salário mínimo
    x1 = vlr_renda_total_fam_SM
    # comprometimento máximo da renda da família:
    comp_max_renda_fam <- fcase(
        x1 <= 0.5, 0.15*(vlr_renda_total_fam_SM*SM),
        x1 > 0.5 & x1 < 3, (0.15 + x1*0.05)*(vlr_renda_total_fam_SM*SM),
        default = 0.3*(vlr_renda_total_fam_SM*SM)
    )
    comp_max_renda_fam
}

# Função para selecionar os imóveis possíveis, dado o nível de comprometimento
# de renda da família:
listaImoveisDisp <- function(cr,
                             aluguel_up,#comp_renda_fam, 
                             aluguel_lw,#subsidio_max,
                             #subsidio_min,
                             file_path = "data/imoveis/") {
    library(arrow)
    library(dplyr)
    # Valor máximo de locação:
    #max_vlr_aluguel = comp_renda_fam + subsidio_max
    # querying parquet dataset:
    imoveis_psb = open_dataset(file_path) %>% 
        filter(preco_aluguel >= aluguel_lw & preco_aluguel <= aluguel_up) %>% 
        collect()
    # calculando:
    imoveis_psb %>% 
        mutate(
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
        arrange(desc(subsidio_efetivo))
}


plotMap <- function(imoveis) { 
    imoveis %>% 
        #head(10) %>% 
        mutate(
            info = paste(
                sep="<br/>", 
                str_c("Aluguel: <b>R$ ", preco_aluguel, "</b>"),
                str_c("Sudsídio: <b>R$ ", subsidio_efetivo, "</b>"),
                endereco,
                str_c(andar, "° Andar"),
                str_c("Área: ", round(area_m2), " M2")
                )
        ) %>% 
        leaflet() %>% 
        addTiles() %>% 
        addMarkers(
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


Código Base de Processamento

# Carregar as bibliotecas necessárias

library(dplyr)

library(lubridate)

library(readxl)

library(writexl)


# Carregar as bases de produção e ativa

base_producao <- read_excel("C:/Users/renan/OneDrive/Documentos/Base Produção.xlsx")

base_ativa <- read_excel("C:/Users/renan/OneDrive/Documentos/Base Ativa.xlsx")

 
# Converter datas em ambos os data.frames

base_producao <- base_producao %>%

  mutate(`Data Início do Contrato Prestamista` = as.Date(as.character(`Data Início do Contrato Prestamista`), format = "%d%m%Y"))

 

base_ativa <- base_ativa %>%

  mutate(`Data Início do Contrato Prestamista` = as.Date(as.character(`Data Início do Contrato Prestamista`), format = "%d%m%Y"))

 

# Função para calcular o prêmio pro-rata

calcula_pro_rata <- function(premio, data_inicio, parcelas) {

  # Garantir os valores corretos

  meses_utilizados <- as.numeric(interval(as.Date(data_inicio), today()) %/% months(1))

  meses_contrato <- as.numeric(parcelas)

 

  # Cálculo do prêmio pro-rata

  ifelse(

    is.na(meses_utilizados) | is.na(meses_contrato) | meses_contrato == 0,

    0, # Caso inválido

    premio * (1 - (meses_utilizados / meses_contrato))

  )

}

 

# Identificar operações de lastro na base de produção

contratos_lastro <- base_producao %>%

  filter(`Tipo de Movimento` == "R" & !is.na(`Operação Lastro`)) %>%

  left_join(base_ativa, by = c("Operação Lastro" = "Contrato"), suffix = c("", "_ativo")) %>%

  mutate(

    # Calcular o prêmio pro-rata

    `Prêmio Pro-rata` = calcula_pro_rata(`Prêmio em Valor_ativo`, `Data Início do Contrato Prestamista_ativo`, Parcelas_ativo),

   

    # Calcular o prêmio ajustado (inclusão + pro-rata)

    `Prêmio Ajustado` = `Prêmio em Valor` + ifelse(is.na(`Prêmio Pro-rata`), 0, `Prêmio Pro-rata`)

  )

 

# Criar linhas de exclusão (duplicar linhas da base ativa associadas ao lastro)

contratos_exclusao <- contratos_lastro %>%

  mutate(

    `Tipo de Movimento` = "E", # Marcar como exclusão

    Status = "Inativo"

  ) %>%

  select(names(base_producao), `Prêmio Pro-rata`, `Tipo de Movimento`, Status)

 

# Criar linhas de inclusão (a nova operação com valores ajustados)

contratos_inclusao <- contratos_lastro %>%

  mutate(

    `Tipo de Movimento` = "I", # Marcar como inclusão

    Status = "Ativo"

  ) %>%

  select(names(base_producao), `Prêmio Ajustado`, `Tipo de Movimento`, Status)

 

# Criar base completa: base ativa original + exclusões + inclusões

base_completa <- bind_rows(base_ativa, contratos_exclusao, contratos_inclusao)

 

# Base ativa limpa: apenas contratos ativos pós-processamento

base_ativa_limpa <- base_completa %>%

  filter(Status == "Ativo") %>%

  distinct(`Contrato`, .keep_all = TRUE)

 

# Base inativa: apenas contratos marcados como exclusão

base_inativa <- base_completa %>%

  filter(Status == "Inativo") %>%

  distinct(`Contrato`, .keep_all = TRUE)

 

# Salvar os resultados em arquivos separados

write_xlsx(base_completa, "base_completa.xlsx")

write_xlsx(base_ativa_limpa, "base_ativa_limpa.xlsx")

write_xlsx(base_inativa, "base_inativa.xlsx")
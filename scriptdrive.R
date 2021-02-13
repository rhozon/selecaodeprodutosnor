
### Script de autenticação para sua conta do Google Drive.

library(googledrive) # Carrega a biblio do Google Drive

drive_find() # Lista todos os arquivos

drive_download("~/dataset_credclientes.csv",
               overwrite = TRUE)

cred_clientes <- read.csv(file="dataset_credclientes.csv", head=TRUE, sep=";") # Lê os dados

str(cred_clientes) # Mostra o dataset


library(dplyr) # Biblioteca pra manipulação de dados

agrupa_alunos_ativos<-cred_clientes%>%
  group_by(cliente_nome)%>%
  summarise(qtd_alunos_ativos = sum(alunos_ativos, na.rm = TRUE))
agrupa_alunos_ativos%>%
  arrange(desc(qtd_alunos_ativos)) # Mostra a academia com mais alunos ativos no período

agrupa_recebiveis<-cred_clientes%>%
  group_by(cliente_nome)%>%
  summarise(soma_recebiveis = sum(alunos_ativos, na.rm = TRUE))
agrupa_recebiveis%>%
  arrange(desc(soma_recebiveis)) # Mostra a academia com mais recebiveis ativos no período

agrupa_conta_pagar<-cred_clientes%>%
  group_by(cliente_nome)%>%
  summarise(soma_conta_pagar = sum(total_conta_pagar, na.rm = TRUE))
agrupa_conta_pagar%>%
  arrange(desc(soma_conta_pagar)) # Mostra a academia com mais contas a pagar no período

agrupa_cc_maquina<-cred_clientes%>%
  group_by(cliente_nome)%>%
  summarise(soma_cc_maquina = sum(total_valor_cartao_credito_maquina, na.rm = TRUE))
agrupa_cc_maquina%>%
  arrange(desc(soma_cc_maquina)) # Mostra a academia com mais uso de cartão de crédito máquina no período

agrupa_cc_recorrencia<-cred_clientes%>%
  group_by(cliente_nome)%>%
  summarise(soma_cc_recorrencia = sum(total_valor_cartao_credito_recorrencia, na.rm = TRUE))
agrupa_cc_recorrencia%>%
  arrange(desc(soma_cc_recorrencia)) # Mostra a academia com mais uso de cartão de crédito recorrencia no período




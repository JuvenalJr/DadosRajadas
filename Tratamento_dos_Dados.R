library(dplyr)

TravisData <- readRDS('TravisData.RDS')
CommitData <- readRDS('CommitData.RDS')


### selecionando colunas do TravisData
TravisData <- TravisData %>% select(
          # Id da Build
          tr_build_id, 
          # statud da build conforme instução 1
          build_successful,
          # id de todos os commits da Build
          git_all_built_commits,
          #nome do projeto
          gh_project_name,
          # id do commit que disparou a Build
          git_trigger_commit
                                    )



########
# instruções de programação
########
# 1 - Pegar tabela com build jobs e agrupar por build (se pelo menos um job falhar, considere que a build falhou)
# 2 - Usar tabela de commits, fazer join com tabela de builds; nessa tabela, cada linha é um commit
# 3 - Descartar commits do tipo merge
# 4 - Aplicar algoritmo de kleinberg para identificar rajadas de commit
# 5 - Para cada commit, devemos saber: se faz parte de uma rajada (sim/não), status da build relacionada ao commit:
########

######## 1- Agrupando por build

TravisData_build <- TravisData %>%


  #agrupando
  group_by(tr_build_id) %>%
  
    summarise(
    
      # Status da build:
      # Se falhou para qualquer job = FALSE
      # else = TRUE
      build_successful = if_else( any(build_successful == FALSE), 
                                  FALSE, TRUE),
    
      # identificador de todos os commits da build
      commits = unique(git_all_built_commits),
      
      # identificador do commit gatilho
      git_trigger_commit= unique(git_trigger_commit),
    
      # Nome do projeto
      gh_project_name = unique(gh_project_name)
      
  )
  
######## 2 - join build commit

# renomeando colunas do dataset Commits

library(data.table) #setnames
setnames(CommitData, old = c('project','sha'), 
                     new = c('gh_project_name','git_commit_id')
        )

# criando uma linha por commit no db TravisData_build

library(tidyr) # unnest()
library(lubridate) # ymd_hms()

TravisCommits <- TravisData_build %>%
  
  # Criando campo com lista de commits
  mutate(git_commit_id = strsplit(commits, "#"))  %>%
  # criando uma linha para cada elemento da lista de commits
  unnest(git_commit_id) %>%
  # juntando os datasets
  inner_join(CommitData, by = c("git_commit_id", "gh_project_name"))  %>% 
  # tratndo datas dos commits como objeto do pacote lubridate
  mutate(date = ymd_hms(date))

######## 3 - Descartar commits do tipo merge

# Duvidas:
# 3.1 - Como descartar os commits do tipo merge?
# 3.1.1 - No final devemos ter apenas um commit por build, ou podem ser mais de um?
# 3.1.2 - Caso seja apenas um, seria o commit mais recente da build, o mais antigo ou o commit que disparou a build?
# 3.1.3 - caso seja mais de um, como diferenciar um merge commit de um commit em uma Build? Eles teriam a mesma data e hms?

######## 4 - Algoritmo de Kleinberg

# Duvidas:
# 4.1 - Agrupar por projeto antes de rodar o algoritmo?
# 4.2 - Qual valor deve ser usado como nível de rajadas?

######## 5 - Para cada commit: rajada(T/F), status(T/F)
  
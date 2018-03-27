library(dplyr)

options(digits.secs=4)

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

str(CommitData$date)

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
#library(lubridate) # ymd_hms()

TravisCommits <- TravisData_build %>%
  
  # Criando campo com lista de commits
  mutate(git_commit_id = strsplit(commits, "#"))  %>%
  # criando uma linha para cada elemento da lista de commits
  unnest(git_commit_id) %>%
  # juntando os datasets
  inner_join(CommitData, by = c("git_commit_id", "gh_project_name"))  %>% 
  # tratndo datas dos commits como objeto do pacote lubridate
  mutate(date = as.POSIXct(date,format = "%Y-%m-%d  %H:%M:%OS") )

#testando comits duplicados

any(duplicated(TravisCommits$git_commit_id))
######## 3 - Descartar commits do tipo merge


######## 4 - Algoritmo de Kleinberg

library(bursts)

# Crinado serie com os nomes dos projetos
projetos <- unique(TravisCommits$gh_project_name)

# selecionado projetos para teste
# comente para selecionar todos os projetos
#projetos <- projetos[c(1,5)]


# selecionado primeiro projeto
#comentar para selecionar todos os projetos
proj.atual <- TravisCommits %>% filter(gh_project_name == projetos[1])

### problema:
### O algoritmo de kleinberg apresenta erro...
#... quando duas ou mais datas são conhecidentes
# rode a lina a baixo para testar
k <- kleinberg(proj.atual$date)
### mensagem de erro:
#Error in kleinberg(proj.atual$date) : 
#Input cannot contain events with zero time between!


#agrupando dados para examinar commits com a mesma data
proj.atual <- proj.atual %>%
  group_by(date, add = TRUE) %>%
  
  summarise(commits = n_distinct(commits),
            
           git_commit_id = n_distinct(git_commit_id),
            
            build_successful = if_else( any(build_successful == FALSE), 
                                        FALSE, TRUE),
            
            gh_project_name = unique(gh_project_name),
            
            tr_build_id = n_distinct(tr_build_id)

                          ) 
# o read mostra que, no projeto selecioado temos até...
#... 10 commits com a mesma data
head(arrange(proj.atual, desc(git_commit_id)), 10)

#ordenando por tr_build temos um commit com até 7 builds
head(arrange(proj.atual, desc(tr_build_id)), 10)


# Duvida:
# 4.1 - Como tratar esses dados?

######## 5 - Para cada commit: rajada(T/F), status(T/F)
  
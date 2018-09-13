library(dplyr)
library(tidyr)
library(data.table)

TravisData <- readRDS('TravisData.rds')

CommitData <- readRDS('CommitData.rds')

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

######## 1- Agrupando por build

TravisData_build <- TravisData %>%
  #agrupando
  group_by(tr_build_id) %>%
  summarise(
    # Status da build:
    # Se falhou para qualquer job = FALSE
    # else = TRUE
    build_successful = if_else( any(build_successful == FALSE), FALSE, TRUE),
    # identificador de todos os commits da build
    commits = unique(git_all_built_commits),
    # identificador do commit gatilho
    git_trigger_commit= unique(git_trigger_commit),
    # Nome do projeto
    gh_project_name = unique(gh_project_name)
  )

######## 2 - join build commit

# renomeando colunas do dataset Commits
setnames(CommitData, old = c('project','sha'), 
         new = c('gh_project_name','git_commit_id')
)


# criando uma linha por commit no db TravisData_build

TravisCommits <- TravisData_build %>%
  # Criando campo com lista de commits
  mutate(git_commit_id = strsplit(commits, "#"))  %>%
  # criando uma linha para cada elemento da lista de commits
  unnest(git_commit_id) %>% # juntando os datasets
  inner_join(CommitData, by = c("git_commit_id", "gh_project_name"))  %>% 
  # tratndo datas dos commits como objeto do pacote lubridate
  mutate(date = as.POSIXct(date,format = "%Y-%m-%d  %H:%M:%OS") )

TravisCommits<-TravisCommits%>% select(tr_build_id,build_successful,git_commit_id,gh_project_name,date,author_email)

save.image("./DadosRajadas.RData")





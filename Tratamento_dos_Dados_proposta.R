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
projetos <- projetos[c(2,5)]

#data frame vazio para amrmazenar os resultados de todos os projetos
total.builds <- data.frame()


####### loop for
for(i in 1:length(projetos)){
  # selecionado primeiro projeto
  #comentar para selecionar todos os projetos
    proj.atual <- TravisCommits %>% filter(gh_project_name == projetos[i])
  
  cat('projeto atual: ',projetos[i],'\n')
  
  # Adiciona um tempo aleatório entre 0 e 1 segundo para evitar registros com data igual,
  # que impediriam o funcionamento do algoritmo de Kleinberg
  proj.atual.old <- proj.atual
  proj.atual <- proj.atual.old %>%
    group_by(date) %>%
    mutate(date2 = date + runif(n())) %>%
    ungroup() %>%
    select(-date) %>%
    rename(date = date2)
  
  
  k <- kleinberg(proj.atual$date)
 # if(nrow(k)>10){
    
  #}
  
  # grafico de rajadas para o projeto atual
  #plot(k)
  cat('numero de niveis ', nrow(unique(k[1])),'\n')
  # nivel escolhido
  #kLevel = 2
  # leveis[1] <- round(quantile(1:nrow(unique(k[1])), 0.25))
  # leveis[2] <- round(quantile(1:nrow(unique(k[1])), 0.50))
  # leveis[3] <- round(quantile(1:nrow(unique(k[1])), 0.75))
  leveis = c(2, 3, 4)
  ######## 5 - Para cada commit: rajada(T/F), status(T/F)
  
  k2 <- subset(k, level == leveis[1])
  # ordenando os breaks
  breaks = sort(c(k2$start, k2$end))
  # criando variavel logica isBurst
  # se o commit pertence a um brust <- TRUE se não <- FALSE
  proj.atual <- proj.atual %>%
    mutate(isBurst1 = cut(date, breaks=breaks, labels=FALSE)) %>%
    mutate(isBurst1 = if_else(is.na(isBurst1), F, isBurst1 %% 2 == 1))
  
  k2 <- subset(k, level == leveis[2])
  # ordenando os breaks
  breaks = sort(c(k2$start, k2$end))
  # criando variavel logica isBurst
  # se o commit pertence a um brust <- TRUE se não <- FALSE
  proj.atual <- proj.atual %>%
    mutate(isBurst2 = cut(date, breaks=breaks, labels=FALSE)) %>%
    mutate(isBurst2 = if_else(is.na(isBurst2), F, isBurst2 %% 2 == 1))
  
  k2 <- subset(k, level == leveis[3])
  # ordenando os breaks
  breaks = sort(c(k2$start, k2$end))
  # criando variavel logica isBurst
  # se o commit pertence a um brust <- TRUE se não <- FALSE
  proj.atual <- proj.atual %>%
    mutate(isBurst3 = cut(date, breaks=breaks, labels=FALSE)) %>%
    mutate(isBurst3 = if_else(is.na(isBurst3), F, isBurst3 %% 2 == 1))
  
  proj.builds <- proj.atual %>% 
    group_by(tr_build_id) %>% 
    summarise(  
      build_successful = unique(build_successful),
      # se ao menos um dos commits é rajada, a build e considerada rajada
      isBurst1 = any(isBurst1),
      isBurst2 = any(isBurst2),
      isBurst3 = any(isBurst3),
      #burst_passed = build_successful & isBurst,
      gh_project_name = unique(gh_project_name)
    )
  
  tab <- xtabs(~ isBurst1 + build_successful, data=proj.builds)
  tab
  mosaicplot(tab, shade=T)
  chisq.test(tab)
  
  tab <- xtabs(~ isBurst2 + build_successful, data=proj.builds)
  tab
  mosaicplot(tab, shade=T)
  chisq.test(tab)
  
  tab <- xtabs(~ isBurst3 + build_successful, data=proj.builds)
  tab
  mosaicplot(tab, shade=T)
  chisq.test(tab)
  
  
  total.builds <- rbind( total.builds ,proj.builds)
  
}
###### fim do loop for

tab <- xtabs(~ isBurst1 + build_successful, data=total.builds)
tab
mosaicplot(tab, shade=T)
chisq.test(tab)

tab <- xtabs(~ isBurst2 + build_successful, data=total.builds)
tab
mosaicplot(tab, shade=T)
chisq.test(tab)

tab <- xtabs(~ isBurst3 + build_successful, data=total.builds)
tab
mosaicplot(tab, shade=T)
chisq.test(tab)

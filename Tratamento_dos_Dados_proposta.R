library(dplyr)
library(tidyr)
library(data.table)
library(bursts)

options(digits.secs=4)

load("./DadosRajadas.RData")

#testando comits duplicados

#any(duplicated(TravisCommits$git_commit_id))
######## 3 - Descartar commits do tipo merge


######## 4 - Algoritmo de Kleinberg

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

  # grafico de rajadas para o projeto atual
  #plot(k)
  cat('numero de niveis ', nrow(unique(k[1])),'\n')
  
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

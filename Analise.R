library(dplyr)
library(tidyr)
library(data.table)
library(bursts)

rm(list=ls())
load("./DadosRajadas.RData")

######## 4 - Algoritmo de Kleinberg

# Crinado serie com os nomes dos projetos
x<-TravisCommits%>% 
  mutate(gh_project_name= gsub ("/"," ",gh_project_name))%>%
  select(tr_build_id,build_successful,git_commit_id,gh_project_name,date,author_email)%>%
  group_by(gh_project_name)%>%
  mutate(n_commits = n(), tempo = difftime(max(as.POSIXct(date)),min(as.POSIXct(date)),units="weeks"))
#projetos<-TravisCommits%>% select (gh_project_name)

projetos<-x %>% group_by(gh_project_name)%>%
  summarise(n_commits=max(n_commits),tempo=max(tempo))
projetos<-projetos%>%filter(tempo>48&n_commits>100&n_commits<105)

  
#data frame vazio para amrmazenar os resultados de todos os projetos
total.builds <- data.frame()


####### loop for
for(i in 1:length(projetos$gh_project_name)){
  rm(list=ls()[!ls()%in%c("i","x","projetos","total.builds")])
  # selecionado primeiro projeto
  #comentar para selecionar todos os projetos
  proj.atual <- x %>% filter(gh_project_name == projetos$gh_project_name[i])
  
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

  tab <- xtabs(~ isBurst1 + build_successful, data= proj.atual)
  chisqlist <- chisq.test(tab)
  
  proj.atual<-proj.atual%>% 
    mutate(pvalue1 = as.numeric(chisqlist$p.value))
  
  
  
    
  k2 <- subset(k, level == leveis[2])
  # ordenando os breaks
  breaks = sort(c(k2$start, k2$end))
  # criando variavel logica isBurst
  # se o commit pertence a um brust <- TRUE se não <- FALSE
  proj.atual <- proj.atual %>%
    mutate(isBurst2 = cut(date, breaks=breaks, labels=FALSE)) %>%
    mutate(isBurst2 = if_else(is.na(isBurst2), F, isBurst2 %% 2 == 1))

  tab <- xtabs(~ isBurst2 + build_successful, data= proj.atual)
  chisqlist <- chisq.test(tab)
  
  proj.atual<-proj.atual%>% 
    mutate(pvalue2 = as.numeric(chisqlist$p.value))
  
  k2 <- subset(k, level == leveis[3])
  # ordenando os breaks
  breaks = sort(c(k2$start, k2$end))
  # criando variavel logica isBurst
  # se o commit pertence a um brust <- TRUE se não <- FALSE
  proj.atual <- proj.atual %>%
    mutate(isBurst3 = cut(date, breaks=breaks, labels=FALSE)) %>%
    mutate(isBurst3 = if_else(is.na(isBurst3), F, isBurst3 %% 2 == 1))
 
  tab <- xtabs(~ isBurst3 + build_successful, data= proj.atual)
  chisqlist <- chisq.test(tab)
  
  proj.atual<-proj.atual%>% 
    mutate(pvalue3 = as.numeric(chisqlist$p.value))
  
   
  proj.builds <- proj.atual %>% 
    group_by(tr_build_id) %>% 
    summarise(  
      build_successful = unique(build_successful),
      # se ao menos um dos commits é rajada, a build e considerada rajada
      isBurst1 = any(isBurst1),
      isBurst2 = any(isBurst2),
      isBurst3 = any(isBurst3),
      # Salvando os valores de P(teste qui-quadrado)
      pvalue1 = max(pvalue1),
      pvalue2 = max(pvalue2),
      pvalue3 = max(pvalue3),
      #burst_passed = build_successful & isBurst,
      gh_project_name = unique(gh_project_name)
    )
 
  total.builds <- rbind( total.builds ,proj.builds)
  
}
rm(list=ls()[!ls()%in%c("x","projetos","total.builds")])
save.image("./DadosAnalise.RData")
###### fim do loop for
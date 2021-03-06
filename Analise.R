library(dplyr)
library(tidyr)
library(data.table)
library(bursts)
library(vcd)

rm(list=ls())
load("./DadosRajadas.RData")

######## 4 - Algoritmo de Kleinberg

# Crinado serie com os nomes dos projetos
x<-TravisCommits%>% 
  mutate(gh_project_name= gsub ("/"," ",gh_project_name))%>%
  select(tr_build_id,build_successful,git_commit_id,gh_project_name,date,author_email, gh_lang,git_diff_src_churn)%>%
  group_by(gh_project_name)%>%
  mutate(n_commits = n(), tempo = difftime(max(as.POSIXct(date),na.rm=TRUE),min(as.POSIXct(date),na.rm=TRUE),units="weeks"))
#projetos<-TravisCommits%>% select (gh_project_name)

projetos<-x %>% group_by(gh_project_name)%>%
  summarise(n_commits=max(n_commits),tempo=max(tempo),gh_lang=max(gh_lang))
projetos<-projetos%>%filter(tempo>48&n_commits>100&n_commits<1000)


#data frame vazio para amrmazenar os resultados de todos os projetos
total.builds <- data.frame()

####### loop for
for(i in 2:length(projetos$gh_project_name)){
  print(i)
  rm(list=ls()[!ls()%in%c("i","x","projetos","total.builds")])
  # selecionado primeiro projeto
  #comentar para selecionar todos os projetos
  proj.atual <- x %>% filter(gh_project_name == projetos$gh_project_name[i])
  
  if(n_distinct( proj.atual$build_successful)>1 ) {
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
    
    if(n_distinct(proj.atual$build_successful)>1 & n_distinct(proj.atual$isBurst1)>1){
      tab <- xtabs(~ isBurst1 + build_successful, data= proj.atual)
      chisqlist <- chisq.test(tab)
      cramer <- assocstats(tab)$cramer
      taxa_sucesso_sem_rajada <- tab[1, 2] / (tab[1, 2] + tab[1, 1])
      taxa_sucesso_com_rajada <- tab[2, 2] / (tab[2, 2] + tab[2, 1])
      
      proj.atual<-proj.atual%>% 
        mutate(pvalue1 = as.numeric(chisqlist$p.value),
               cramer1 = cramer,
               tx_sem_burst1 = taxa_sucesso_sem_rajada,
               tx_com_burst1 = taxa_sucesso_com_rajada,
               razao_com_sem_burst1 = tx_com_burst1 / tx_sem_burst1)
    } else{
      proj.atual<-proj.atual%>% 
        mutate(pvalue1 = NA)
      next
    } 
    
    
    
    k2 <- subset(k, level == leveis[2])
    # ordenando os breaks
    breaks = sort(c(k2$start, k2$end))
    # criando variavel logica isBurst
    # se o commit pertence a um brust <- TRUE se não <- FALSE
    proj.atual <- proj.atual %>%
      mutate(isBurst2 = cut(date, breaks=breaks, labels=FALSE)) %>%
      mutate(isBurst2 = if_else(is.na(isBurst2), F, isBurst2 %% 2 == 1))
    
    if(n_distinct(proj.atual$build_successful)>1 & n_distinct(proj.atual$isBurst2)>1){
      
      tab <- xtabs(~ isBurst2 + build_successful, data= proj.atual)
      chisqlist <- chisq.test(tab)
      cramer <- assocstats(tab)$cramer
      taxa_sucesso_sem_rajada <- tab[1, 2] / (tab[1, 2] + tab[1, 1])
      taxa_sucesso_com_rajada <- tab[2, 2] / (tab[2, 2] + tab[2, 1])
      
      proj.atual<-proj.atual%>% 
        mutate(pvalue2 = as.numeric(chisqlist$p.value),
               cramer2 = cramer,
               tx_sem_burst2 = taxa_sucesso_sem_rajada,
               tx_com_burst2 = taxa_sucesso_com_rajada,
               razao_com_sem_burst2 = tx_com_burst2 / tx_sem_burst2)
    } else{
      proj.atual<-proj.atual%>% 
        mutate(pvalue2 = NA)
      next
    } 
    
    k2 <- subset(k, level == leveis[3])
    # ordenando os breaks
    breaks = sort(c(k2$start, k2$end))
    # criando variavel logica isBurst
    # se o commit pertence a um brust <- TRUE se não <- FALSE
    proj.atual <- proj.atual %>%
      mutate(isBurst3 = cut(date, breaks=breaks, labels=FALSE)) %>%
      mutate(isBurst3 = if_else(is.na(isBurst3), F, isBurst3 %% 2 == 1))
    
    if(n_distinct(proj.atual$build_successful)>1 & n_distinct(proj.atual$isBurst3)>1){
      
      tab <- xtabs(~ isBurst3 + build_successful, data= proj.atual)
      chisqlist <- chisq.test(tab)
      cramer <- assocstats(tab)$cramer
      taxa_sucesso_sem_rajada <- tab[1, 2] / (tab[1, 2] + tab[1, 1])
      taxa_sucesso_com_rajada <- tab[2, 2] / (tab[2, 2] + tab[2, 1])
      
      proj.atual<-proj.atual%>% 
        mutate(pvalue3 = as.numeric(chisqlist$p.value),
               cramer3 = cramer,
               tx_sem_burst3 = taxa_sucesso_sem_rajada,
               tx_com_burst3 = taxa_sucesso_com_rajada,
               razao_com_sem_burst3 = tx_com_burst3 / tx_sem_burst3)
    } else{
      proj.atual<-proj.atual%>% 
        mutate(pvalue3 = NA)
      next
    }  
    
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
        #
        cramer1 = max(cramer1),
        tx_sem_burst1 = max(tx_sem_burst1),
        tx_com_burst1 = max(tx_com_burst1),
        razao_com_sem_burst1 = max(razao_com_sem_burst1),
        
        cramer2 = max(cramer2),
        tx_sem_burst2 = max(tx_sem_burst2),
        tx_com_burst2 = max(tx_com_burst2),
        razao_com_sem_burst2 = max(razao_com_sem_burst2),
        
        cramer3 = max(cramer3),
        tx_sem_burst3 = max(tx_sem_burst3),
        tx_com_burst3 = max(tx_com_burst3),
        razao_com_sem_burst3 = max(razao_com_sem_burst3),
        #burst_passed = build_successful & isBurst,
        gh_project_name = unique(gh_project_name),
        gh_lang = unique(gh_lang),
        git_diff_src_churn = max(git_diff_src_churn)
      )
    
    total.builds <- rbind( total.builds ,proj.builds)
  }
  
}

otal.builds<- total.builds%>%filter(gh_lang!="javascript")

resultadotestes<- total.builds%>%
  group_by(gh_project_name)%>%
  summarise(pvalue1=max(pvalue1),
            pvalue2=max(pvalue2),
            pvalue3=max(pvalue3),
            cramer1 = max(cramer1),
            tx_sem_burst1 = max(tx_sem_burst1),
            tx_com_burst1 = max(tx_com_burst1),
            razao_com_sem_burst1 = max(razao_com_sem_burst1),
            
            cramer2 = max(cramer2),
            tx_sem_burst2 = max(tx_sem_burst2),
            tx_com_burst2 = max(tx_com_burst2),
            razao_com_sem_burst2 = max(razao_com_sem_burst2),
            
            cramer3 = max(cramer3),
            tx_sem_burst3 = max(tx_sem_burst3),
            tx_com_burst3 = max(tx_com_burst3),
            razao_com_sem_burst3 = max(razao_com_sem_burst3),
            
            gh_lang = unique(gh_lang),
            git_diff_src_churn = max(git_diff_src_churn)
  )%>%
  mutate(nivel1 = if_else(pvalue1>=0.05,1,0),
         nivel2 = if_else(pvalue2>=0.05,1,0), 
         nivel3 = if_else(pvalue3>=0.05,1,0))

aux1<-resultadotestes%>%select(nivel1,gh_lang,git_diff_src_churn)%>%mutate(nivel = "nivel 1",hipotese=nivel1)%>%select(nivel,hipotese,gh_lang,git_diff_src_churn)
aux2<-resultadotestes%>%select(nivel2,gh_lang,git_diff_src_churn)%>%mutate(nivel = "nivel 2",hipotese=nivel2)%>%select(nivel,hipotese,gh_lang,git_diff_src_churn)
aux3<-resultadotestes%>%select(nivel3,gh_lang,git_diff_src_churn)%>%mutate(nivel = "nivel 3",hipotese=nivel3)%>%select(nivel,hipotese,gh_lang,git_diff_src_churn)

resultadotestes2<-rbind(aux1,aux2,aux3)

#Gráfico final com o resultado de todos os níveis. 
ww<-xtabs(~hipotese+nivel, data=resultadotestes2)


rm(list=ls()[!ls()%in%c("x","projetos","total.builds","resultadotestes","resultadotestes2","ww")])
save.image("./DadosAnalise.RData")
###### fim do loop for

#######################

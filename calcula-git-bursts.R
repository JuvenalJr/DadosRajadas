rm(list=ls())
library(dplyr)
library(lubridate)
library(bursts)

commits <- readRDS("commits.rds")


#' Estamos calculando rajadas a nivel de commit.
#' Será que deveremos calcular a nivel de build (i.e., de push)?

#' - CommitData: cada commit com id, data, mensagem, autor
#' - TravisData: cada build job, com projeto, lista de commits e trigger commit
#' - TravisData_build: igual ao de cima, mas agrupa por build
#' 
#' - TravisCommits: join de CommitData com TravisData_build


#' Determina número de commits e duração de cada projeto
all_projects <- commits %>%
  group_by(project) %>%
  summarise(n_commits = n(),
            durationWeeks = difftime(max(authorDate, na.rm=T), min(authorDate, na.rm=T), units = "weeks"))
nrow(all_projects)

#' - Seleciona projetos com entre 100 e 5000 commits e pelo menos 1 ano de duração
projects <- all_projects %>%
  filter(durationWeeks >= 52 &
           n_commits >= 100 & n_commits <= 5000)
nrow(projects)

#' 
#' ----------------------------------------------------------
#' 
#' Analisa cada projeto
#' 

#' Cria tabela para guardar rajadas (carrega do arquivo .rds se existir)
#' Isso permite salvarmos os resultados à medida que forem sendo gerados
n <- nrow(projects)
if (file.exists("bursts.rds")) {
  all_bursts <- readRDS("bursts.rds")
} else {
  all_bursts <- data.frame(project = projects$project,
             level = integer(n),
             start = .POSIXct(rep(NA, n)),
             end = .POSIXct(rep(NA, n)),
             stringsAsFactors = F)
}

while (TRUE) {
  #' Seleciona o primeiro projeto da lista
  #' Se já tiver com rajadas cadastradas, sai
  if (!is.na(all_bursts[1, "start"])) {
    break
  }
  selected_project <- all_bursts[1, "project"]
  print(selected_project)

  #' No caso de commits com a mesma data de autoria, seleciona aquele com
  #' menor data de commit
  selected_commits <- commits %>%
    filter(project == selected_project) %>%
    arrange(commitDate) %>%
    group_by(authorDate) %>%
    filter(row_number() == 1)
  
  #' Calcula rajadas usando o algoritmo de Kleinberg
  k <- kleinberg(selected_commits$authorDate)
  result <- cbind(project = selected_project, k)
  
  #' Insere as rajadas do projeto no final da tabela de rajadas
  all_bursts <- rbind(all_bursts[-1, ], result)
  
  saveRDS(all_bursts, "bursts.rds")
}

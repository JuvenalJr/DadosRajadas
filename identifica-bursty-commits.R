rm(list=ls())
library(dplyr)

all_bursts <- readRDS("bursts.rds")
all_commits <- readRDS("commits.rds") %>%
  filter(project %in% all_bursts$project)

#' Verifica se todos os instantes inicial e final aparecem como data do commit 
all(all_bursts$start %in% all_commits$authorDate)
all(all_bursts$end %in% all_commits$authorDate)

#' Identifica características dos projetos quanto às rajadas
all_projects <- all_bursts %>%
  group_by(project) %>%
  summarise(levels = max(level))
min(all_projects$levels)
hist(all_projects$levels)

#' Vamos usar level 4, que é o menor nível presente em todos os projetos
#' (se removermos um projeto)
selected_level <- 4
projects <- all_projects %>%
  group_by(project) %>%
  filter(levels >= selected_level)

#' 
commits <- all_commits[0, ] %>% cbind(isBurst = logical(0))

for (selected_project_name in projects$project) {
  print(selected_project_name)
  project_bursts <- all_bursts %>%
    filter(project == selected_project_name,
           level == selected_level) %>%
    arrange(start, end)
  
  breaks <- sort(c(project_bursts$start, project_bursts$end))
  
  project_commits <- all_commits %>%
    filter(project == selected_project_name) %>%
    mutate(interval = cut(authorDate, breaks = breaks, labels = FALSE)) %>%
    mutate(isBurst = if_else(is.na(interval), F, interval %% 2 == 1)) %>%
    select(-interval)
  
  commits <- rbind(commits, project_commits)
}

commits <- commits %>%
  mutate(date = authorDate)

saveRDS(commits, "commits-bursts.rds")

library(dplyr)
library(readr)
library(lubridate)

CURDIR <- getwd()
print(CURDIR)
DIR <- file.path(CURDIR, "gitrepos")
projects <- scan("projects.txt", character())

dados <- NULL

for (project in projects) {
  print(project)
  proj_dir <- file.path(CURDIR, "gitrepos", gsub("/", "__", project))
  setwd(proj_dir)
  
  cmd = "git --no-pager log --no-merges --format=\"%H|%ae|%aI|%cI\""
  result <- system(cmd, intern=T) %>% paste(collapse = "\n")
  csv <- read.csv(textConnection(result), sep="|", header = F, col.names = c("commit", "author", "authorDate", "commitDate"), stringsAsFactors = F)
  csv <- csv %>%
    mutate(project = project) %>%
    select(project, commit, author, authorDate, commitDate)
  if (is.null(dados)) {
      dados <- csv
  } else {
    dados <- rbind(dados, csv)
  }
}

setwd(CURDIR)

dados <- dados %>%
  mutate_at(vars(authorDate, commitDate), ymd_hms) %>%
  mutate_if(is.factor, as.character) %>%
  filter(!is.na(authorDate))

saveRDS(dados, "commits.rds")

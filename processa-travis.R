library(dplyr)
library(tidyr)
library(data.table)

#' Cada linha é um build job
all_travis <- readRDS('TravisData.rds')
summary(all_travis)
table(all_travis$tr_status)

jobs <- all_travis %>%
  filter(tr_status != 'canceled') %>%
  mutate(buildOk = tr_status == "passed") %>%
  select(
    project = gh_project_name,
    language = gh_lang,
    build = tr_build_id,
    buildOk,
    commits = git_all_built_commits,
    src_churn = git_diff_src_churn,
    core_team = gh_by_core_team_member)

builds <- jobs %>%
  group_by(build) %>%
  summarise(
    project = unique(project),
    language = unique(language),
    buildOk = all(buildOk),
    commits = unique(commits),
    src_churn = unique(src_churn),
    core_team = unique(core_team)
  )

saveRDS(builds, "builds.rds")

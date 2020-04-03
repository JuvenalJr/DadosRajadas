rm(list=ls())
library(dplyr)
library(tidyr)

commits <- readRDS("commits-bursts.rds")
orig_builds <- readRDS("builds.rds")

builds <- orig_builds %>%
  separate_rows(commits, sep = "#") %>%
  rename(commit = commits) %>%
  inner_join(commits, by = c("project", "commit")) %>%
  group_by(build) %>%
  summarise(
    project = first(project),
    language = first(language),
    buildOk = first(buildOk),
    commits = paste0(commit, collapse="#"),
    src_churn = first(src_churn),
    core_team = all(core_team),
    author = first(author),
    date = first(date),
    isBurst = any(isBurst))

saveRDS(builds, "builds-bursts.rds")

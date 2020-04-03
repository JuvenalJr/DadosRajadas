CURDIR <- getwd()
print(CURDIR)
DIR <- file.path(CURDIR, "gitrepos")
projects <- scan("projects.txt", character())

setwd(DIR)


for (project in projects) {
  # print(project)
  proj_dir <- gsub("/", "__", project)
  if (!dir.exists(proj_dir)) {
    url <- paste0("https://github.com/", project)
    # cmd = "git clone --filter=blob:none --no-checkout --single-branch --branch master"
    cmd = "git clone --filter=blob:none --no-checkout"
    full_cmd = paste(cmd, url, proj_dir)
    # system(full_cmd)
    print(full_cmd)
  }
}

setwd(CURDIR)

# -----------

# library(dplyr)
# library(tidyr)
# commits <- readRDS("commits.rds")
# x <- TravisData_build %>%
#   separate_rows(commits, sep = "#")
# 
# y <- x %>%
#   filter(gh_project_name == "rails/rails") %>%
#   left_join(commits, by = c("commits" = "commit")) %>%
#   filter(author %>% is.null())
# 
#   left_join()
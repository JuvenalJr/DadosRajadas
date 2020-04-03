rm(list=ls())

library(dplyr)
library(lsr)
library(effsize)
library(ggplot2)

builds <- readRDS("builds-bursts.rds")

# Risco relativo de y = TRUE, comparando x = TRUE e x = FALSE
# Interpretação: quando x = TRUE, a probabilidade de y = TRUE é aumentada em rr vezes
#             ou quando x = FALSE, ...
risk_ratio <- function(tab) {
  prop <- prop.table(aux, margin = 1)
  rr <- (prop[2, 2] / prop[1, 2])
  if (rr < 1) {
    rr <- 1 / rr
  }
  rr
}

dados_full <- builds %>%
  filter(language != "javascript") %>%
  mutate(
    changesCode = src_churn > 0,
    firstBuild = is.na(lag(project)) | lag(project) != project,
    prevBuildOk = if_else(firstBuild, NA, lag(buildOk)),
    didFix = if_else(firstBuild, NA, !prevBuildOk & buildOk),
    didBreak = if_else(firstBuild, NA, prevBuildOk & !buildOk)) %>%
  filter(!firstBuild)  # remove 1a build

dados_apos_verde <- dados_full %>%
  filter(prevBuildOk)

dados_apos_vermelho <- dados_full %>%
  filter(!prevBuildOk)







#' ### Caracterização

mean(builds$isBurst)
mean(dados_full$buildOk)

# http://www.real-statistics.com/chi-square-and-f-distributions/effect-size-chi-square/


#' Após builds quebradas, o conserto é mais frequentemente feito
#' sem alterar o código-fonte
aux <- xtabs(~ changesCode + didFix, data = dados_apos_vermelho)
mosaicplot(aux, shade=T)
cramersV(aux)
risk_ratio(aux)

#' ---------------------------------------------------------------------------
#' 
#' ### Builds resultantes de rajadas de commit possuem maior propensão a quebrar?

aux <- xtabs(~ isBurst + buildOk, data=dados_full)
chisq.test(aux)
mosaicplot(aux, shade = T)
cramersV(aux)
risk_ratio(aux)
# rr = risco relativo


#' ### Builds resultantes de rajadas de commit possuem maior propensão a quebrar
#' ### uma build originalmente verde?

aux <- xtabs(~ isBurst + buildOk, data=dados_apos_verde)
mosaicplot(aux, shade = T)
chisq.test(aux)
cramersV(aux)
risk_ratio(aux)

#' A diferença é negligível.
#' Isso sugere que as rajadas não são uma causa das builds quebradas, e 
#' sim uma consequência: quando uma build é quebrada, os desenvolvedores
#' priorizam consertar a build, e podem precisar fazer vários commits pequenos
#' até conseguir


#' ---------------------------------------------------------------------------
#' Após uma build quebrada, é mais provável ter uma rajada de commits
aux <- xtabs(~ prevBuildOk + isBurst, data=dados_full)
mosaicplot(aux, shade = T)
chisq.test(aux)
cramersV(aux)
risk_ratio(aux)

#' ---------------------------------------------------------------------------
#' Após uma build quebrada, 

dados_full %>% 
  ggplot(aes(x=prevBuildOk, y=src_churn)) +
  geom_boxplot() +
  scale_y_log10()
wilcox.test(src_churn ~ prevBuildOk, data = dados_full)
cliff.delta(src_churn ~ prevBuildOk, data = dados_full)


#' Dado que foi feita uma rajada, o churn influencia no resultado da build?
aux <- xtabs(~ changesCode + buildOk, data=filter(dados_full, isBurst))
chisq.test(aux)
mosaicplot(aux, shade = T)
cramersV(aux)
risk_ratio(aux)

#' Dado que foi feita uma rajada, o churn influencia no resultado da build?
aux <- xtabs(~ core_team + buildOk, data=filter(dados_full, isBurst))
chisq.test(aux)
mosaicplot(aux, shade = T)
cramersV(aux)
risk_ratio(aux)


aux <- xtabs(~ core_team + buildOk, data=dados_full)
chisq.test(aux)
mosaicplot(aux, shade = T)
cramersV(aux)
risk_ratio(aux)

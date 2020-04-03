rm(list=ls())

library(dplyr)
library(lsr)
library(ggplot2)
library(effsize)

load("./DadosAnalise.RData")

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

# se ao menos um dos commits faz parte de uma rajada, a build e considerada rajada
# kleinberg: https://nikkimarinsek.com/blog/kleinberg-burst-detection-algorithm
#            https://www.cs.cornell.edu/home/kleinber/bhs.pdf
# Rajadas são períodos em que a frequência de commits é muito maior do que a
#   frequência média de um projeto. Por isso, em um projeto pode ser
#   1 commit por minuto, em outro pode ser 1 commit por dia...
# No algoritmo de kleinberg, isso é controlado pelos parâmetros
#    s (frequencias de um estado) e gamma (custo de transição de estado)
#    ?bursts::kleinberg
#    The value of s may be modified; higher values increase the strictness of the algorithm's criterion for how dramatic an increase of activity has to be to be considered a ‘burst.’
#    Higher values mean roughly that bursts must be sustained over longer periods of time in order for the algorithm to recognize them.
# O algoritmo é recursivo: dentro de uma rajada, ele pode ser executado
#    novamente para detectar uma rajada de mais alto nível
# Na dissertação, explicar o algoritmo fazendo uma simulação com um
#    exemplo pré-fabricado
# As variaveis isBurst1/2/3 na verdade representam os níveis 2/3/4

# TODO: arquivo Analise.R:
#   git_diff_src_churn = max(git_diff_src_churn) ### TODO: deve ser soma!!!

###############################

dados_full <- projetos %>%
  inner_join(total.builds, by = c("gh_project_name", "gh_lang")) %>%
  filter(gh_lang != "javascript") %>%
  mutate(isBurst = isBurst3) %>%
  select(project = gh_project_name, lang = gh_lang, build = tr_build_id, isBurst, churn = git_diff_src_churn, buildOk = build_successful) %>%
  mutate(
         changesCode = churn > 0,
         firstBuild = is.na(lag(project)) | lag(project) != project,
         prevBuildOk = if_else(firstBuild, NA, lag(buildOk)),
         didFix = if_else(firstBuild, NA, !prevBuildOk & buildOk),
         didBreak = if_else(firstBuild, NA, prevBuildOk & !buildOk)) %>%
  filter(!firstBuild)  # remove 1a build

dados_full_exceto_1a_build <- dados_full %>%
  filter(!is.na(prevBuildOk))

dados_apos_verde <- dados_full %>%
  filter(prevBuildOk)

dados_apos_vermelho <- dados_full %>%
  filter(!prevBuildOk)


#' ### Caracterização

mean(total.builds$isBurst1)
mean(total.builds$isBurst2)
mean(total.builds$isBurst3)

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
prop <- prop.table(aux, margin = 1)
chisq.test(aux)
mosaicplot(aux, shade = T)
cramersV(aux)
ris
rr <- 1 / (prop[2, 2] / prop[1, 2])
rr
# rr = risco relativo

#' Foi observada uma diferença estatisticamente significativa, mostrando que
#' builds resultantes de rajadas estão mais propensas a serem quebradas.
#' De fato, essas builds possuem 10.7% maior probabilidade serem quebradas.
#' No entanto, o tamanho do efeito é pequeno (V de Cramer < 0.10)
#' (Sugestão: focar o relato do risco relativo, desfocar do V de Cramer)

#' ---------------------------------------------------------------------------
#' 
#' ### Builds resultantes de rajadas de commit possuem maior propensão a quebrar
#' ### uma build originalmente verde?

aux <- xtabs(~ isBurst + buildOk, data=dados_apos_verde)
prop <- prop.table(aux, margin = 1)
chisq.test(aux)
mosaicplot(aux, shade = T)
cramersV(aux)
rr <- 1 / (prop[2, 2] / prop[1, 2])
rr

#' A diferença é negligível.
#' Isso sugere que as rajadas não são uma causa das builds quebradas, e 
#' sim uma consequência: quando uma build é quebrada, os desenvolvedores
#' priorizam consertar a build, e podem precisar fazer vários commits pequenos
#' até conseguir

#' ---------------------------------------------------------------------------
#' Após uma build quebrada, é mais provável ter uma rajada de commits
aux <- xtabs(~ prevBuildOk + isBurst, data=dados_full)
prop <- prop.table(aux, margin = 1)
chisq.test(aux)
mosaicplot(aux, shade = T)
cramersV(aux)
rr <- 1 / (prop[2, 2] / prop[1, 2])
rr

#' De fato, é 27% mais provável (mas o tamanho do efeito é pequeno)

#' ---------------------------------------------------------------------------
#' Após uma build quebrada, 

dados_full_exceto_1a_build %>% 
  ggplot(aes(x=prevBuildOk, y=churn)) +
  geom_boxplot() +
  scale_y_log10()
wilcox.test(churn ~ prevBuildOk, data = dados_full_exceto_1a_build)
cliff.delta(churn ~ prevBuildOk, data = dados_full_exceto_1a_build)


#' Dado que foi feita uma rajada, o churn influencia no resultado da build?
aux <- xtabs(~ changesCode + buildOk, data=filter(dados_full, isBurst))
chisq.test(aux)
mosaicplot(aux, shade = T)
cramersV(aux)
risk_ratio(aux)


#' Builds resultantes de rajadas de commits possuem maior propensão a
#' consertar uma build quebrada?

# aux <- xtabs(~ isBurst + build_successful, data=dados_apos_quebrada)
# chisq.test(aux)
# mosaicplot(aux, shade = T)
# cramersV(aux)

#' ### Commits que modificam maior número de linhas possuem maior propensão a
#' ### quebrar uma build verde?
# https://rdrr.io/cran/effsize/man/cliff.delta.html

dados_apos_verde %>% ggplot(aes(x=buildOk, y=churn)) +
  geom_boxplot() +
  scale_y_log10()
wilcox.test(churn ~ buildOk, data=dados_apos_verde)
cliff.delta(churn ~ buildOk, data=dados_apos_verde)

#' Existe uma diferença negligível entre 


#' -----------------------------
#' TODO: variable importance

aux <- xtabs(~ isBurst + gh_lang + build_successful, data=dados)
library(vcd)
library(relaimpo)

library(caret)
library(xgboost)
dados_factor <- dados %>%
  mutate(build_successful = factor(build_successful, labels=c("yes", "no")))
control <- trainControl(method="repeatedcv", number=10, repeats=3)
regressor=train(build_successful ~ isBurst + gh_lang, data=sample_n(dados_factor, 20), method = "adaboost", trControl = control)
varImp(regressor)


mm <- calc.relimp(formula = build_successful ~ isBurst, data=dados_factor, type = "lmg")
# http://r-statistics.co/Variable-Selection-and-Importance-With-R.html




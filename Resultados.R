# Nivel 1

## Quantos são estatisticamente significativos?
mean(resultadotestes$nivel1)

## Tendência (aumenta/diminui a taxa de sucesso quando tem rajada)
significativos <- resultadotestes %>%
  filter(nivel1 == 1) %>%
  mutate(tendencia = if_else(razao_com_sem_burst1 > 1, 'aumenta', 'diminui'))
pie(table(significativos$tendencia))

## Tam do efeito
hist(significativos$cramer1)

# Nivel 2

## Quantos são estatisticamente significativos?
mean(resultadotestes$nivel2)

## Tendência (aumenta/diminui a taxa de sucesso quando tem rajada)
significativos <- resultadotestes %>%
  filter(nivel2 == 1) %>%
  mutate(tendencia = if_else(razao_com_sem_burst2 > 1, 'aumenta', 'diminui'))
pie(table(significativos$tendencia))

## Tam do efeito
hist(significativos$cramer2)

# Nivel 3

## Quantos são estatisticamente significativos?
mean(resultadotestes$nivel3)

## Tendência (aumenta/diminui a taxa de sucesso quando tem rajada)
significativos <- resultadotestes %>%
  filter(nivel3 == 1) %>%
  mutate(tendencia = if_else(razao_com_sem_burst3 > 1, 'aumenta', 'diminui'))
pie(table(significativos$tendencia))

## Tam do efeito
hist(significativos$cramer3)

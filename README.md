# Rajadas Builds

<hr>

#### Bases de Dados

Os arquivos **TravisData.RDS** e **CommitData.RDS** foram gerados pelo script **SQLITE_CSV_To_RDS.R** a partir das bases encontradas no link:

[https://drive.google.com/open?id=0ByfKQshHHBo8ZTZaTGJxNW9xb2c](https://drive.google.com/open?id=0ByfKQshHHBo8ZTZaTGJxNW9xb2c)

<hr>

#### Arquivo principal

O arquivo **Tratamento_dos_Dados.R** executa as etapas 1 e 2 das orientaÃ§Ãµes a partir dos bancos de dados TravisData.RDS e CommitData.RDS

<hr>

 #### Dúvidas sobre as partes 3 e 4:

##### Parte 3:

 3.1 - Como descartar os commits do tipo merge?

 3.1.1 - No final devemos ter apenas um commit por build, ou podem ser mais de um?

 3.1.2 - Caso seja apenas um, seria o commit mais recente da build, o mais antigo ou o commit que disparou a build?

 3.1.3 - caso seja mais de um, como diferenciar um merge commit de um commit em uma Build? Eles teriam a mesma data e hms?

 ##### Parte 4:

4.1 - Agrupar por projeto antes de rodar o algoritmo?

4.2 - Qual valor deve ser usado como nível de rajadas?

<hr>

#### Orientações:

1. Pegar tabela com build jobs e agrupar por build (se pelo menos um job falhar, considere que a build falhou)
- Usar tabela de commits, fazer join com tabela de builds; nessa tabela, cada linha Ã© um commit
- Descartar commits do tipo merge
- Aplicar algoritmo de kleinberg para identificar rajadas de commit
- Para cada commit, devemos saber: se faz parte de uma rajada (sim/nÃ£o), status da build relacionada ao commit:


| id_commit | faz_parte_de_rajada? | build_passou? |
|---- |--- |---- |
| 1 | TRUE | FALSE |

<hr>

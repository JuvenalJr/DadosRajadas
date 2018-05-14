library(RSQLite)

TravisData <- read.csv("travistorrent_8_2_2017.csv",header = TRUE ,sep = ",")

# Transformando fatores em strings
i <- sapply(TravisData, is.factor)
TravisData[i] <- lapply(TravisData[i], as.character)

# conectando banco de dados sqlite
db <- dbConnect(SQLite(), dbname ="commits.sqlite" )

#lista de dataframes no banco de dados
dbListTables(db)

#gravando dataframe comit como dataframe 
CommitData <- dbGetQuery(db, "select * from commits")

#disconectando
dbDisconnect(db)

#salvando arquivos rds
saveRDS(CommitData, file = "CommitData.rds")
saveRDS(TravisData, file = "TravisData.rds")

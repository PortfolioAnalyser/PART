# library(devtools)
# install_github("VirtuaGod/AnaliseFI")

# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T

# A função getFI_best() tem um argumento "Begin_End" com o formato "2007-12-31/2017-01-20".# Caso não seja colocada tem default em
# 1999-12-31/2017-12-31. Cria objectos XTS de todos os Morningstar Ids (versão "pura" das cotações da morningstar) e um objecto "Fundos"
# em dataframe, com todas as cotações dos fundos alinhadas. É uma versão "alterada" das cotações que acrescenta dias, de forma alinhar
# as cotações). Usa a feed do Best por isso só funciona com fundos que eles comercializam.

getFI_best <- function(x, Begin_End = "1999-12-31/2017-12-31"){
  Fundos <- as.data.frame(timeBasedSeq(Begin_End))
  colnames(Fundos) <- ("date")
  Fundos$date <- as.Date(Fundos$date, format = "%Y-%m-%d")
  Begin <- str_sub(Begin_End, start = 1, end = 10)
  Begin <- as.Date(Begin, format = "%Y-%m-%d")
  Begin <- strptime(as.character(Begin), "%Y-%m-%d")
  Begin <- format(Begin, "%m/%d/%Y")

  for (i in x) {
    fund <- read.csv2(paste0("https://lt.morningstar.com/api/rest.svc/timeseries_price/okhysb8aoh?id=",i,
                             "&currencyId=BAS&idtype=Morningstar&frequency=daily&startDate=", Begin,"&outputType=CSV"))
    fund <- fund[, 1:2]
    fund$date <- as.Date(fund$date, format = "%Y-%m-%d")
    colnames(fund) <- paste(c("date", i))
    Fundos <- merge(Fundos, fund, by = "date", all.x = TRUE)
    fund   <- xts(fund[2:ncol(fund)], order.by = fund[, 1])
    fund   <- fund[Begin_End, ]
    assign('Fundos', Fundos, envir = .GlobalEnv)
    assign(i, fund, envir = .GlobalEnv)
    rm(fund)
  }
  Fundos <- na.locf(Fundos)
  Fundos <- xts(Fundos[2:ncol(Fundos)], as.Date(Fundos[, 1], format = "%Y-%m-%d"))
  Fundos <- Fundos[.indexwday(Fundos) %in% 1:5] # Para excluir os fim de semana
  Fundos <- as.data.frame(Fundos)
  date <- row.names(Fundos)
  Fundos <- cbind(date, Fundos)
  assign('Fundos', Fundos, envir = .GlobalEnv)
}


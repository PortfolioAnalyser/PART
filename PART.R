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

getFI_best <- function(x, Begin_End = "1999-12-31/2017-12-31") {

  Fundos <- as.data.frame(timeBasedSeq(Begin_End))
  colnames(Fundos) <- ("date")
  Fundos$date <- as.Date(Fundos$date, format = "%Y-%m-%d")
  Begin <- str_sub(Begin_End, start = 1, end = 10)
  Begin <- as.Date(Begin, format = "%Y-%m-%d")
  Begin <- strptime(as.character(Begin), "%Y-%m-%d")
  Begin <- format(Begin, "%m/%d/%Y")
  for (i in x) {
    fund <- read.csv2(paste0("https://lt.morningstar.com/api/rest.svc/timeseries_price/okhysb8aoh?id=",
                             i, "&currencyId=BAS&idtype=Morningstar&frequency=daily&startDate=",
                             Begin, "&outputType=CSV"))
    fund <- fund[, 1:2]
    fund$date <- as.Date(fund$date, format = "%Y-%m-%d")
    colnames(fund) <- paste(c("date", i))
    Fundos <- merge(Fundos, fund, by = "date", all.x = TRUE)
    fund <- xts(fund[2:ncol(fund)], order.by = fund[, 1])
    fund <- fund[Begin_End, ]
    assign(i, fund, envir = .GlobalEnv)
    rm(fund)
  }
  Fundos <- na.locf(Fundos)
  Fundos <- xts(Fundos[2:ncol(Fundos)], as.Date(Fundos[, 1],
                                                format = "%Y-%m-%d"))
  Fundos <- Fundos[.indexwday(Fundos) %in% 1:5]
  Fundos <- as.data.frame(Fundos)
  date <- row.names(Fundos)
  Fundos <- cbind(date, Fundos)
  Fundos  <- data.frame(lapply(Fundos, as.character), stringsAsFactors = FALSE)
  Fundos[, 1] <- ymd(Fundos[, 1])
  Fundos.num <- Fundos
  Fundos.num <- data.frame(lapply(Fundos.num, as.numeric), stringsAsFactors = FALSE)
  Fundos.xts <- xts(Fundos.num, order.by = Fundos[, 1])[, -1]
  Fundos.df <- cbind(Fundos[, 1], (Fundos.num))
  Fundos.df <- Fundos.df[, -2]
  colnames(Fundos.df)[1] <- 'date'
  rownames(Fundos.df) <- seq(1,nrow(Fundos.df), 1)
  assign("Fundos.xts", Fundos.xts, envir = .GlobalEnv)
  assign("Fundos.df", Fundos.df, envir = .GlobalEnv)
}

#### Functions for the package ####
empty.df.like <- function(x){
  # Makes an empty data.frame with the same number of columns and
  # rows as another data.frame
  data.frame(matrix(NA, nrow = nrow(x), ncol = ncol(x)))
}

empty.df.like.rows <- function(x){
  # Makes an empty data.frame with the same number of rows of another
  # data.frame but 0 columns
  data.frame(matrix(NA, nrow = nrow(x), ncol = 0))
}

normalize <- function(x){
  # Normalizes a vector by dividing every value by the first
  # Creates a Normalized Price Chart
  x / x[1]
}

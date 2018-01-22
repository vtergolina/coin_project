# OBJETIVO: CRIPTOMOEDA ESTÁ DISPONÍVEL NO BINANCE?
setwd("/home/bgrillob/criptoBot/")
library(rvest)
library(magrittr)
library(tm)
# IMPORTAR TWEET E BASE REFERÊNCIA
baseRef <- read.table("MoedasBinance.csv", sep = ";", stringsAsFactors = FALSE)
fileName <- "texto.txt"
textoTweet <- readChar(fileName, file.info(fileName)$size)

palavrasTweet <- textoTweet %>%
  tolower() %>%
  gsub(pattern = "[\r\n]", replacement = " ", x = .) %>%
  removePunctuation() %>%
  removeWords(., stopwords("english")) %>%
  removeNumbers() %>%
  stripWhitespace() %>%
  strsplit(split = " ") %>%
  unlist() %>%
  toupper() %>%
  data.frame(
    Pair = ., 
    Comprimento = nchar(.), 
    stringsAsFactors = FALSE
  ) %>%
  filter(
    Comprimento >= 3 & Comprimento <= 5
  )
  
resultado <- merge(palavrasTweet, baseRef)
resultado <- ifelse(nrow(resultado) == 0, NA, resultado$Pair[1])
print(resultado)

# https://dev.socrata.com/foundry/healthdata.gov/anag-cw7u
# https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/anag-cw7u

library(jsonlite)
library(dplyr)
library(data.table)

options(scipen=999)

# key <- '6l5yhlftarqr49qws6sttwbb2'
# secret <- '3lg47jxlnvl29or8xiyd6enkcj350gdb04ufe7p3wpga3tcmkt'

limit <- 50000
url <- paste0('https://healthdata.gov/resource/anag-cw7u.json?state=FL&$limit=',limit)
offset <- 0
l <- list()
i <- 1

while (TRUE) {
  fullurl <- paste0(url,'&$offset=',offset)
  print(paste0('Processing ',fullurl))
  d <- fromJSON(fullurl)
  d2 <- d[,!(names(d) %in% c('geocoded_hospital_address'))]
  l[[i]] <- d2
  
  if(nrow(d) < limit) break
  offset <- offset + limit
  i <- i+1
}

full <- rbindlist(
  l = l,
  fill = T
)
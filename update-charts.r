source('process.r')
# devtools::install_github("munichrocker/DatawRappr",ref="master")

library(DatawRappr)

print("Starting chart updater")

startdate <- gsub(
  pattern = " 0",
  replacement = ' ',
  x = format(
    x = max(fullwithifips$collection_week_formatted,na.rm=T),
    format = "%B %d"
  )
)

enddate <- gsub(
  pattern = " 0",
  replacement = ' ',
  x = format(
    x = max(fullwithifips$collection_week_formatted,na.rm=T)+6,
    format = "%B %d"
  )
)

chartIDs <- c(
  'mS15D' # pbc icu beds in use/free
)

apikey <- Sys.getenv("DATAWRAPPER_API")

for (id in chartIDs) {
  print(id)
  dw_edit_chart(
    chart_id = id,
    api_key = apikey,
    annotate = paste0("Latest data covers ",startdate," through ",enddate,'.')
  )
  print("Publishing chart")
  dw_publish_chart(
    chart_id = id,
    api_key = apikey,
    return_urls = TRUE
  )
}

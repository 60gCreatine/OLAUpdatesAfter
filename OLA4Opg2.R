Bilscrape <- readRDS("bilbasenWebScrape_2024-11-26-20-49.rds")
Forhandler <- readRDS("bilbasenWebScrapeForhandler_2024-11-28-01-12.rds")

colnames(Bilscrape)[7] <- "scrape_time"

library(RMariaDB)
library(dplyr)

readRenviron(".Renviron")
password <- Sys.getenv("password")

con <- dbConnect(MariaDB(),
                 dbname = "Bilbasen",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = password)



# Opdel Bilbasen til forskellige DFs
car_details <- Bilscrape[, c(
  "carid",
  "model",
  "description",
  "specificmodel",
  "link",
  "registrering",
  "kilometertal",
  "rækkevidde",
  "brændstof")]

dealer_details <- Bilscrape[, c(
  "forhandlerid",
  "by",
  "region")]

sales_details <- Bilscrape[, c(
  "carid",
  "pris",
  "model",
  "specificmodel",
  "link",
  "registrering",
  "forhandlerid",
  "scrape_time"
)]
sales_details$prisid <- NA
sales_details$sold <- NA

# Datasæt fjernet
Bilscrape1 <- readRDS("Dkbiler.rds")

# Opdel Bilbasen til forskellige DFs
car_details_newest <- Bilscrape1[, c(
  "carid",
  "model",
  "description",
  "specificmodel",
  "link",
  "registrering",
  "kilometertal",
  "rækkevidde",
  "brændstof")]

dealer_details_newest <- Bilscrape1[, c(
  "forhandlerid",
  "by",
  "region")]

sales_details_newest <- Bilscrape1[, c(
  "carid",
  "pris",
  "model",
  "specificmodel",
  "link",
  "registrering",
  "forhandlerid",
  "scrape_time"
)]
sales_details_newest$prisid <- NA
sales_details_newest$sold <- NA
dbWriteTable(con,"car_details",car_details, overwrite = TRUE)
dbWriteTable(con,"dealer_details",dealer_details, overwrite = TRUE)
dbWriteTable(con,"sales_details",sales_details, overwrite = TRUE)




carid_query <- paste0("SELECT carid FROM sales_details")
old_carid <- dbGetQuery(con,carid_query)
sales_details_newest$sold <- as.integer(!(sales_details_newest$carid %in% old_carid$carid))

result <- anti_join(sales_details_newest,sales_details, by = "carid")


dbWriteTable(con, "sales_details", sales_details, append = T)




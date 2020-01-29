#Load packages
library(tidyverse)
library(tidyusafec)
library(tigris)
library(sf)
library(censusapi)


#load API keys from env file
save_datagov_apikey(key = Sys.getenv("FEC_API_KEY"))
census_key = Sys.getenv("CENSUS_KEY")

#tell tigris we're working with shape files
options(tigris_class = "sf")

## create a function that does the opposite of `%in%`
`%notin%` <- Negate(`%in%`)

#pull the data we want from the API
kellyContribsRaw <- search_candidates(name = c("KELLY, MARK"), 
                           election_year = "2020", 
                           office ="S",
                           candidate_status = "C",
                           has_raised_funds = TRUE, 
                           unnest_committees = TRUE) %>% 
  get_itemized_contributions(data_structure = "tidy") %>% 
  unnest_wider(committee, names_repair = "unique") %>% 
  select(schedule_type_full, 
         report_type, 
         line_number, 
         line_number_label, 
         contributor_state, 
         is_individual, 
         report_year, 
         contributor_city, 
         contribution_receipt_date, 
         contributor_aggregate_ytd, 
         two_year_transaction_period, 
         contribution_receipt_amount, 
         contributor_zip, 
         contributor_name, 
         state, 
         party, 
         committee_type_full, 
         state_full, 
         name, 
         fec_election_type_desc, 
         memo_code) %>%
filter(report_year > 2018 & line_number %in% "11AI" & memo_code %notin% "X" )

#select further specific data for graphics in a new dataframe
filterKelly <- kellyContribsRaw %>% 
  select(report_type, 
         contributor_name,
         contributor_city,
         contributor_state,
         contributor_zip,
         contribution_receipt_amount,
         contribution_receipt_date)
#preserve zeroes of zipcodes, then chop last five numbers
  filterKelly$contributor_zip <- as.character(filterKelly$contributor_zip)
  filterKelly$contributor_zip <- substr(filterKelly$contributor_zip, 1, 5)
  
######################################################################################
##       there are three 00000 zipcodes, here are the images for them:              ##
##       Pio, J https://docquery.fec.gov/cgi-bin/fecimg/?201910159164670517         ##
##       Judy, Todd https://docquery.fec.gov/cgi-bin/fecimg/?201904159146387245     ##
##       Swift, Susan https://docquery.fec.gov/cgi-bin/fecimg/?201910159164670481   ##
######################################################################################  
  
kellyTotals <- filterKelly %>% 
  group_by(contributor_zip) %>% 
  summarise(total_raised = sum(contribution_receipt_amount))

#get shapefile
az <- zctas("AZ", cb=T)
cong_tot <- left_join(az, totals , by = c("ZCTA5CE10"= "contributor_zip" ))



### code below is in progress/broken
## trying to figure out how to call population by zcta
geography <- listCensusMetadata(name = "acs/acs5", vintage = 2017, type = "geography")
variables <- listCensusMetadata(name = "acs/acs5", vintage = 2017, type = "variables")
apis <- listCensusApis()


#call should be something along this line
population <- getCensus(name = "acs/acs5",
                        vintage = 2017,
                        group = "B01003",
                        vars = c("B01003_001E", "B01003_001M","B01003_001MA","B01003_001EA", "GEOID"),
                        region = "state:04", "zip code tabulation area:*",
                        regionin = "state:04",
                        key=census_key)

### example of what worked previously
ohio_pop_2016 <- getCensus(name = "pep/population",
                           vintage = 2016,
                           vars = c("GEONAME", "POP"),
                           region = "county:*",
                           regionin = "state: 39")



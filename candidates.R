#Load packages
library(tidyverse)
library(tidyusafec)
library(tigris)
library(sf)
library(leaflet)
library(censusapi)

#load API keys from env file
save_datagov_apikey(key = Sys.getenv("FEC_API_KEY"))
census_key = Sys.getenv("CENSUS_API_KEY")

#tell tigris we're working with shape files
options(tigris_class = "sf")

## create a function that does the opposite of `%in%`
`%notin%` <- Negate(`%in%`)

#pull the data we want from the API
apiContribs <- search_candidates(name = c("MCSALLY, MARTHA", "KELLY, MARK"),
                                 election_year = "2020", 
                                 office ="S",
                                 candidate_status = "C",
                                 has_raised_funds = TRUE, 
                                 unnest_committees = TRUE) %>% 
  get_itemized_contributions(data_structure = "tidy") %>% 
  unnest_wider(committee, names_repair = "unique") %>% 
  select(schedule_type_full, 
         report_type, 
         entity_type,
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
  filter(report_year > 2018 & 
           line_number %in% "11AI" & 
           memo_code %notin% "X" & 
           is_individual == TRUE & 
           entity_type %in% "IND")


# create a dataframe of in-state contributions
instate_funds <- apiContribs %>% 
  select(report_type, 
         contributor_name,
         contributor_city,
         contributor_state,
         contributor_zip,
         contribution_receipt_amount,
         contribution_receipt_date) %>% 
  filter(contributor_state=="AZ")
#preserve zeroes of zipcodes, then chop last five numbers
instate_funds$contributor_zip <- as.character(instate_funds$contributor_zip)
instate_funds$contributor_zip <- substr(instate_funds$contributor_zip, 1, 5)

# import zcta crosswalk csv, then join on zip codes
# link for reference https://www.udsmapper.org/zcta-crosswalk.cfm
crosswalk <- read_csv("data/zip_to_zcta_2019.csv")
crosswalked_contribs <- left_join(instate_funds, crosswalk, by=c("contributor_zip" = "ZIP_CODE"))


## run these to learn about the census api
# geography <- listCensusMetadata(name = "acs/acs5", vintage = 2017, type = "geography")
# variables <- listCensusMetadata(name = "acs/acs5", vintage = 2017, type = "variables")
# apis <- listCensusApis()
# call population data from the census (this calls population for all zctas)
population <- getCensus(name = "acs/acs5",
                        vintage = 2018,
                        vars = c("B01003_001E", "GEO_ID"),
                        region = "zip code tabulation area:*",
                        key = census_key)

#rename population data column population$B01003_001E to "TOTAL_POPULATION"
population <- rename(population, 
                     total_population=B01003_001E) 

# create a new dataframe by joining population data to instate_funds data, dropping any zips that don't match.
az_zcta <- left_join(crosswalked_contribs, population, by=c("ZCTA" = "zip_code_tabulation_area"))

#normalize the totals column by population, then multiply by a number that seems to make sense for the population
### NOTE: this dropped zips 85341 and 85726 with population of 0, totaling $1135 in contributions across five donors.
### These addresses were listed as PO Boxes on receipts.
#### Normalization by 100  people also creates a situation where zip 86003 w/ pop. of 3 has given nearly 37K, creating a big outlier, next nearest 1K
normalized_az <- mutate(az_zcta,
                        normalized_total=(contribution_receipt_amount/total_population)*100)

#get the total raised per zcta
az_totals <- normalized_az %>% 
  group_by(ZCTA, name) %>% 
  summarise(total_raised = sum(normalized_total))

# write_csv(az_totals, "output/az_totals_yearend.csv")

#round normalized_total to two decimals
az_totals <- az_totals %>% 
  mutate(total_raised=round(total_raised, digits=2))

#removes Inf total raised, which are numbers that couldn't be computed (divisons by zero)
az_totals <- az_totals %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

#separate candidates into their own dataframes
mcsally_instate <- filter(az_totals, name=="MCSALLY FOR SENATE INC" )
kelly_instate <- filter(az_totals, name=="MARK KELLY FOR SENATE" )

# export csvs for qgis
write_csv(mcsally_instate, "output/mcsallyInstate_final.csv")
write_csv(kelly_instate, "output/kellyInstate_final.csv")

### Get out of state funds ###
outstate_funds <- apiContribs %>%
  select(name, 
         report_type,
         contributor_name,
         contributor_city,
         contributor_state,
         contributor_zip,
         contribution_receipt_amount,
         contribution_receipt_date) %>%
  filter(contributor_state != "AZ")
#preserve zeroes of zipcodes, then chop last five numbers
outstate_funds$contributor_zip <- as.character(outstate_funds$contributor_zip)
outstate_funds$contributor_zip <- substr(outstate_funds$contributor_zip, 1, 5)

state_totals <- outstate_funds %>%
  group_by(contributor_state, name) %>%
  summarise(total_raised = sum(contribution_receipt_amount)) %>% 
  filter(contributor_state %notin% c("AE", "GU", "PR", "ZZ", "AP", "DC")) #filters out territories, other countries, etc. -- should we leave DC?

#separate the candidates
mcsally_outstate <- filter(state_totals, name=="MCSALLY FOR SENATE INC")
kelly_outstate <- filter(state_totals, name=="MARK KELLY FOR SENATE")

#export for graphics
write_csv(mcsally_outstate, "output/mcsallyOutstate_final.csv")
write_csv(kelly_outstate, "output/kellyOutstate_final.csv")


### --- misc queries --- ###
by_city <- instate_funds %>% ## arizona only
  group_by(contributor_city, name) %>% 
  summarise(total_raised = sum(contribution_receipt_amount))

by_date <- apiContribs %>% 
  group_by(contribution_receipt_date, name) %>% 
  summarise(total_raised = sum(contribution_receipt_amount))

##### ----- census data ---- ####
# TODO: use instate funds here

#Load packages
library(tidyverse)
library(tidyusafec)
library(tigris)
library(sf)
library(leaflet)
library(censusapi)
library(DT)

#load API keys from env file
save_datagov_apikey(key = Sys.getenv("FEC_API_KEY"))
census_key = Sys.getenv("CENSUS_API_KEY")


## create a function that does the opposite of `%in%`
`%notin%` <- Negate(`%in%`)

#pull the data we want from the API
apiContribs <- search_candidates(name = c("KELLY, MARK","MCSALLY, MARTHA"),
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
  select(name, 
         report_type, 
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

# filter out erroneous 00001 zip
crosswalked_contribs <- filter(crosswalked_contribs, contributor_zip != "00001")

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
datatable(az_zcta)
#write_csv(az_zcta, "output/march3/az_raw_contribs_population.csv")

## move zero population zctas to their nearest neighbor. 
az_zcta_movement <- az_zcta

az_zcta_movement <- az_zcta_movement %>% 
  mutate(az_zcta_movement$ZCTA = replace(az_zcta_movement$ZCTA, ZCTA == '85726', '85731'))

#normalize the totals column by population
normalized_az <- mutate(az_zcta,
                        normalized_total=(contribution_receipt_amount/total_population)*100)

raw_az_totals <- az_zcta %>% 
  group_by(ZCTA, name) %>% 
  summarise(total_raised = sum(contribution_receipt_amount))

#get the total raised per zcta
az_totals <- normalized_az %>% 
  group_by(ZCTA, name) %>% 
  summarise(total_raised = sum(normalized_total))

#round normalized_total to two decimals
az_totals <- az_totals %>% 
  mutate(total_raised=round(total_raised, digits=2))

#removes Inf total raised, which are numbers that couldn't be computed (divisons by zero)
## includes kelly zips 85341 and 85726 totaling $1135
## and mcsally zip 85726 totaling $11,900 from three contributors
az_totals <- az_totals %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))


#find the places where mcsally out-raised kelly
candidate_comparison_az <-az_totals %>% 
  spread(name, total_raised) %>% 
  rename(mcsally_raised="MCSALLY FOR SENATE INC", 
         kelly_raised="MARK KELLY FOR SENATE") %>% 
  mutate(marthavsmark = sum(mcsally_raised-kelly_raised))
write_csv(candidate_comparison_az, "output/march3/candidate_fundraising_comparison.csv")

#separate candidates into their own dataframes
mcsally_instate <- filter(az_totals, name=="MCSALLY FOR SENATE INC" )
kelly_instate <- filter(az_totals, name=="MARK KELLY FOR SENATE" )

# export csvs for qgis
write_csv(mcsally_instate, "output/march3/mcsallyInstate_final.csv")
write_csv(kelly_instate, "output/march3/kellyInstate_final.csv")


### Get out of state funds ###
outstate_funds <- apiContribs %>%
  select(name, 
         report_type,
         contributor_name,
         contributor_city,
         contributor_state,
         contributor_zip,
         contribution_receipt_amount,
         contribution_receipt_date)
#preserve zeroes of zipcodes, then chop last five numbers
outstate_funds$contributor_zip <- as.character(outstate_funds$contributor_zip)
outstate_funds$contributor_zip <- substr(outstate_funds$contributor_zip, 1, 5)

state_totals <- outstate_funds %>%
  group_by(contributor_state, name) %>%
  summarise(total_raised = sum(contribution_receipt_amount)) %>% 
  filter(contributor_state %notin% c("AE", "GU", "PR", "ZZ", "AP")) 

#separate the candidates
mcsally_outstate <- filter(state_totals, name=="MCSALLY FOR SENATE INC")
kelly_outstate <- filter(state_totals, name=="MARK KELLY FOR SENATE")


#export for graphics
write_csv(mcsally_outstate, "output/march3/mcsallyOutstate_final.csv")
write_csv(kelly_outstate, "output/march3/kellyOutstate_final.csv")


### --- misc queries --- ###
by_city <- instate_funds %>% ## arizona only
  group_by(contributor_city, name) %>% 
  summarise(total_raised = sum(contribution_receipt_amount))
write_csv(by_city, "output/city_funding.csv")

by_date <- apiContribs %>% 
  group_by(contribution_receipt_date, name) %>% 
  summarise(total_raised = sum(contribution_receipt_amount))

##### ----- census data ---- ####

## run these to learn about the census api
# geography <- listCensusMetadata(name = "acs/acs5", vintage = 2017, type = "geography")
# variables <- listCensusMetadata(name = "acs/acs5", vintage = 2017, type = "variables")
# apis <- listCensusApis()

# get median age data from census
age_table <- getCensus(name="acs/acs5",
                       vintage = 2018,
                       vars = c("B01002_001E"),
                       region = "zip code tabulation area:*",
                       key = census_key)

#join to candidate data
age_table <- left_join(az_totals, age_table, by=c("ZCTA" = "zip_code_tabulation_area"))

#rename from census table name
age_table <- rename(age_table,
                median_age=B01002_001E) 

#get median income data from census
income_table <- getCensus(name="acs/acs5",
                          vintage = 2018,
                          vars = c("B19013_001E"),
                          region = "zip code tabulation area:*",
                          key = census_key)
income_table <- left_join(az_totals, income_table, by=c("ZCTA" = "zip_code_tabulation_area"))

#rename from census table name
income_table <- rename(income_table,
                       median_income=B19013_001E) 

#get race data from census
#use this per RL
race_table <- getCensus(name="acs/acs5/profile",
                        vintage = 2018,
                        vars = c("DP05_0070PE", 
                                 "DP05_0071PE",	
                                 "DP05_0077PE", 	
                                 "DP05_0078PE", 
                                 "DP05_0079PE", 
                                 "DP05_0080PE"), 
                        region = "zip code tabulation area:*",
                        key = census_key)

race_table <- left_join(az_totals, race_table, by=c("ZCTA" = "zip_code_tabulation_area"))

#rename from census table names
race_table <- rename(race_table,
                     total_population=DP05_0070PE,
                     hispanic_latino=DP05_0071PE,
                     white=DP05_0077PE,
                     black=DP05_0078PE,
                     american_indian=DP05_0079PE,
                     asian=DP05_0080PE) 


write_csv(income_table, "output/income.csv")
write_csv(race_table, "output/race.csv")
write_csv(age_table, "output/age.csv")


states <- apiContribs %>%
  group_by(contributor_state, name) %>%
  summarise(total_raised = sum(contribution_receipt_amount))

county_donors <- apiContribs %>%
  mutate(instate_outstate = ifelse(contributor_state %in% "AZ", "Arizona", "out of state")) %>%
  select(name, report_type ,contributor_state, instate_outstate, contributor_city, contributor_zip, contributor_name, contribution_receipt_amount)

in_out_state <- county_donors %>%
  group_by(name, instate_outstate) %>%
  summarise(total_raised = sum(contribution_receipt_amount))

by_county <- county_donors %>%
  group_by(name, contributor_city) %>%
  summarise(total_raised = sum(contribution_receipt_amount))
  
#queries below were areas identified by Randy Leonard as areas north of Tucson and Phoenix that voted for trump but donated more to Kelly than McSally. 

phoenix_area <- az_zcta %>%
  group_by(name) %>%
  filter( contributor_zip %in% c("85024",
                                 "85027",
                                 "85022",
                                 "85083",
                                 "85050",
                                 "85306",
                                 "85308",
                                 "85310",
                                 "85345",
                                 "85381")) %>%
 summarise(total_raised = sum(contribution_receipt_amount))

oro_area <- az_zcta %>%
  group_by(name) %>%
  filter( contributor_zip %in% c("85737",
                                 "85739",
                                 "85742",
                                 "85755")) %>%
  summarise(total_raised = sum(contribution_receipt_amount))

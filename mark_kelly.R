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


# create a dataframe of in-state contributions
instate_funds <- kellyContribsRaw %>% 
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


#create a dataframe of out-of-state contributions
outstate_funds <- kellyContribsRaw %>% 
  select(report_type, 
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
  
###############################      NOTE      #######################################
##                                                                                  ##
##       there are three 00000 zipcodes, here are the images for them:              ##
##       Pio, J https://docquery.fec.gov/cgi-bin/fecimg/?201910159164670517         ##
##       Judy, Todd https://docquery.fec.gov/cgi-bin/fecimg/?201904159146387245     ##
##       Swift, Susan https://docquery.fec.gov/cgi-bin/fecimg/?201910159164670481   ##
##                                                                                  ##
######################################################################################  
  
state_totals <- outstate_funds %>% 
  group_by(contributor_state) %>% 
  summarise(total_raised = sum(contribution_receipt_amount))


## run these to learn about the census api
# geography <- listCensusMetadata(name = "acs/acs5", vintage = 2017, type = "geography")
# variables <- listCensusMetadata(name = "acs/acs5", vintage = 2017, type = "variables")
# apis <- listCensusApis()


#call should be something along this line
population <- getCensus(name = "acs/acs5",
                          vintage = 2018,
                          vars = c("B01003_001E", "GEO_ID"),
                          region = "zip code tabulation area:*") 

#TODO: combine population data w/ arizona data, dropping all the zctas we don't need 


#TODO: pull shapefile for arizona


#TODO: map arizona zcta contributions


#TODO: export out-of-state data for bar chart via graphics rig. this only requires top five states and totals
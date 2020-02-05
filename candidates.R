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

#separate candidates into their own dataframes
mcsallyAPIdata <- filter(apiContribs, name=="MCSALLY FOR SENATE INC" )
kellyAPIdata <- filter(apiContribs, name=="MARK KELLY FOR SENATE" )

#pull in raw data from csvs
mcsallyRaw <- read_csv("data/mcsally_2019_Q1_to_YE.csv",
                       col_types = cols(contributor_organization_name = col_character()),
                       trim_ws = FALSE)

kellyRaw <- read_csv("data/kelly_2019_q1_to_ye.csv",
                       col_types = cols(contributor_organization_name = col_character()),
                       trim_ws = FALSE)


#filter out what I don't want from those csvs
mcsallyRaw <- filter(mcsallyRaw, filing_id == "YE", ## YE only
                     entity_type == "IND", ## Independent contributions only
                     contribution_purpose_descrip == "CONTRIBUTION") ## no transfers or other types of receipts

kellyRaw <- filter(kellyRaw, filing_id == "YE", ## YE only
                     entity_type %in% c("IND"), ## Independent contributions only
                     memo_code %notin% "X") # prevents PACs, committees and other potential duplicates via earmarks

# drop YE from Kelly's api data
kellyAPIdata <-filter(kellyAPIdata, report_type != "YE")

rename(mcsallyRaw)

#bind the datasets together forever in holy matrimony

#Load packages
library(tidyverse)
library(tidyusafec)
library(tigris)
library(sf)
library(censusapi)

#load API keys from env file
save_datagov_apikey(key = Sys.getenv("FEC_API_KEY"))
census_key = Sys.getenv("CENSUS_API_KEY")

#tell tigris we're working with shape files
options(tigris_class = "sf")

## create a function that does the opposite of `%in%`
`%notin%` <- Negate(`%in%`)


#pull the data we want from the API
rawContribs <- search_candidates(name = c("KELLY, MARK"), 
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
instate_funds <- rawContribs %>% 
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
outstate_funds <- rawContribs %>% 
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

az_totals <- instate_funds %>% 
  group_by(contributor_zip) %>% 
  summarise(total_raised = sum(contribution_receipt_amount))

## run these to learn about the census api
# geography <- listCensusMetadata(name = "acs/acs5", vintage = 2017, type = "geography")
# variables <- listCensusMetadata(name = "acs/acs5", vintage = 2017, type = "variables")
# apis <- listCensusApis()


#replace az_totals with crosswalked data 
# link for reference https://www.udsmapper.org/zcta-crosswalk.cfm
crosswalk <- read_csv("data/zip_to_zcta_2019.csv")
az_totals <- left_join(az_totals, crosswalk, by=c("contributor_zip" = "ZIP_CODE"))

# call population data from the census (this calls population for all zctas)
population <- getCensus(name = "acs/acs5",
                          vintage = 2018,
                          vars = c("B01003_001E", "GEO_ID"),
                          region = "zip code tabulation area:*",
                          key = "6ca1427d5d740735f295c8fc411c95119b1100c9")

#rename population data column population$B01003_001E to "TOTAL_POPULATION"
population <- rename(population, 
                     total_population=B01003_001E) 

# create a new dataframe by joining population data to instate_funds data, dropping any zips that don't match.
az_zcta <- left_join(az_totals, population, by=c("ZCTA" = "zip_code_tabulation_area"))

# write_csv(az_zcta, "output/az_totals_zcta.csv")


#TODO: figure out if this normalization was correct -- (contributions/zcta population)*10K
normalized_az <- mutate(az_zcta, 
                        normalize_population=(total_raised/total_population)*100)
# write_csv(normalized_az, "output/normalized_az.csv")


# download arizona sf data from tigris
# az_sf <- zctas(starts_with = c("85", "86"))
az_sf <- zctas(cb = FALSE, year = 2010, state = "AZ")


# join that sf data with normalized_az df
 map_data <- left_join(normalized_az, az_sf,
                       by=c("contributor_zip" = "ZCTA5CE10"))

# write_csv(map_data, "output/az_map_data.csv") ## trying to export for qgis

#TODO: map arizona zcta contributions
# arizona_map <- ggplot(map_data) +
#   geom_sf(data = map_data) +
#   aes(fill= map_data$total_raised) +
#   geom_sf(color="black")  +
#   # scale_fill_manual(values = c("#FFFFFF", "#C9C5DB", "#938CB8", "#3E386D")) +
#   theme_void() +
#   labs(title="Mark Kelly's contributions by zcta per 10k", color='legend', fill='legend title')
# arizona_map

bins <- c(0, 1000, 5000, 20000, 100000, 500000, 750000, Inf)
pal1 <- colorBin("inferno", domain = cong_tot$total_raised_no_na, bins = bins)
map <- leaflet(cong_tot) %>% addTiles()
state_popup1 <- paste0("<strong> County: </strong>", 
                       cong_tot$NAME, 
                       "<br><strong>Total Raised: </strong>", 
                       cong_tot$total_raised_no_na)
leaflet(data = cong_tot) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal1(total_raised_no_na), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup1) %>%
  addLegend("bottomright", pal = pal1, values = ~total_raised_no_na,
            title = "Total raised",
            labFormat = labelFormat(prefix = " "))


#TODO: pull state by state population data -- (state contributions/state populations)/100k



#TODO: export out-of-state data for bar chart via graphics rig. this only requires top ten states and totals



#### Stashing leaflet code here for further work this weekend


#leaflet works much faster and it gives us a more interactive graphic, which i'm partial to.

bins <- c(0, 100, 1000, 10000, 100000, 200000, Inf)
pal1 <- colorBin(palette = c("#FFFFFF", "#C9C5DB","#05B69C", "#F9A51A", "#C73D49"), domain = cong_tot$total_raised_no_na, bins = bins) 
map <- leaflet(cong_tot) %>% addTiles()
state_popup1 <- paste0("<strong> District: </strong>", 
                       cong_tot$ZCTA5CE10, 
                       "<br><strong>Total Raised: </strong>", 
                       cong_tot$total_raised_no_na)
leaflet(data = cong_tot) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal1(total_raised_no_na), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup1) %>%
  addLegend("bottomright", pal = pal1, values = ~total_raised_no_na,
            title = "Total raised",
            labFormat = labelFormat(prefix = " "))



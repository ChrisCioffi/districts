#Access data from the openFEC api using R wrapper from Stephen Holzman 
#https://stephenholzman.github.io/tidyusafec/articles/intro.html

library(tidyverse)
library(tidyusafec)
#great primer on tidyusafec here: https://stephenholzman.github.io/tidyusafec/articles/search-functions.html#search_candidates /// more or less full documentation here: https://rdrr.io/github/stephenholzman/tidyusafec/man/get_candidate_totals.html
library(tigris)
library(leaflet)

#signup for api key at https://api.open.fec.gov/developers/. Save a one line file called "data.gov.key" in the root project folder, that one line assigning the key to a variable like the next line:
save_datagov_apikey(key = "n3BB27dCbHpsI0BAIyYi5i4nMa3xJk9AXF7cG2Hc")



#select all candidates running for Senate, unnest the data, deliver it in a df, and make sure they raised money 
#For North Carolina races
#senate <- search_candidates(state = "NC", election_year = "2020", office = "S", candidate_status = "C" , has_raised_funds = TRUE, unnest_committees = TRUE )  %>%
#for alaska races (this is useful because the file is somewhat small..... See my sampling script at the bottom for a way to take a radom sample to make testing much speedier)
#senate <- search_candidates(state = "AK", election_year = "2020", office = "S", candidate_status = "C" , has_raised_funds = TRUE, unnest_committees = TRUE )  %>%
#for colorado races
#senate <- search_candidates(state = "CO", election_year = "2020", office = "S", candidate_status = "C" , has_raised_funds = TRUE, unnest_committees = TRUE )  %>%
#for arizona races

#### Here's the queries I've been using for counties
#senate <- search_candidates(name = c("MCSALLY, MARTHA"), election_year = "2020", office = "S", candidate_status = "C" , has_raised_funds = TRUE, unnest_committees = TRUE )  %>% 
#I've split these into two queries, because my key currently only allows for 1,000 queries in an hour. This is the one for Kelly. When we make the maps, we can address this.
senate <- search_candidates(name = c("KELLY, MARK"), election_year = "2020", office = "S", candidate_status = "C" , has_raised_funds = TRUE, unnest_committees = TRUE )  %>% 
  #get all their itemized contributions
  get_itemized_contributions(data_structure = "tidy") %>%
  #unnests the pesky committee column and creates unique columns for each of the nested list items.
  #here's a great tutorial on how to get rid of nested lists https://cfss.uchicago.edu/notes/simplify-nested-lists/ Thanks Andrew Tran for pointing me to this
  #something else he suggested. I didn't need it, but am putting it here for safe keeping https://jennybc.github.io/purrr-tutorial/ls00_inspect-explore.html
  unnest_wider(committee, names_repair = "unique") %>%
  #select only the fields I want.
  select(schedule_type_full, report_type, line_number, line_number_label, contributor_state, is_individual, report_year, contributor_city, contribution_receipt_date, contributor_aggregate_ytd, two_year_transaction_period, contribution_receipt_amount, contributor_zip, contributor_name, state, party, committee_type_full, state_full, name, fec_election_type_desc) %>%
  # itemized individual contributions are recorded on line 11ai & we want to get the contributions given in the last 2-year cycle
  filter(report_year > 2018 & line_number %in% "11AI")
#preserve the zeroes
senate$contributor_zip <- as.character(senate$contributor_zip)
#Rural/urban codes were downloaded from https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx
#fec data accessed from https://classic.fec.gov/disclosurep/PDownload.do
#subsets the first 5 numbers of the zipcodes row https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/substr
senate$contributor_zip <- substr(senate$contributor_zip, 1, 5)
#FYI, if you want the last 5, use https://www.rdocumentation.org/packages/FedData/versions/1.1.0/topics/substrRight

##########


#############
#let's read in the counties dataset and the fips code datasets so we can crosswalk the itemized donor zip codes into the counties shapefile  
counties <- read_csv("ZIP_COUNTY.csv")
#crosswalk available here https://www.huduser.gov/portal/datasets/usps_crosswalk.html#codebook
codes <- read_csv("fips.csv") %>%
#
  set_names("fips_code", "county_name","state_abbrev")
#props to Dhmontgomery in newsnerdery. to helping me eliminate the excess dupes by using a combination of rank and filter...basically what top_n does. But with the added ability of adding the ties.method element. #I was able to select the highest ratios and then, in the case of ties, R randomly chose one. 
nodupes <- counties %>% 
  group_by(zip) %>%
  mutate(rank = rank(tot_ratio, ties.method = "random")) %>% 
  filter(rank < 2)
#joining the McSally or Kelly donors with the crosswalk that's got no duplicates
nodupe_donors <- left_join(senate, nodupes , by = c("contributor_zip"= "zip" ))
#now let's join the county fips codes with the names of the counties so we know what the actual name of the county is...  it's what's provided in the shapefile
county_donors <- left_join(nodupe_donors, codes, by = c("county" = "fips_code"))
  

#let's see what and where these senators are getting their funding. This query is just for Tillis donations... and I need to go back and make sure there's no pacs in it. Right now it's set up just to make the map work.

#create a new comlumn that differentiates the AZ counties from other counties. This is so we don't get duplicate counties. Which I odn't think is a thing, but you never know
county_donors <- county_donors %>%
  mutate(instate_outstate = ifelse(contributor_state %in% "AZ", "Arizona", "out of state"))
#generates a total for each county
totals <- county_donors %>%
  #this filter changes based on the race
  #filter(name %in% "THOM TILLIS COMMITTEE") %>%
  #filter(name %in% "ALASKANS FOR DAN SULLIVAN") %>%
#make sure just Arizona counties get in the final results
  filter(instate_outstate %in% "Arizona") %>%
#group the contributions by the county
  group_by(county_name) %>%
#summarize the totals
  summarise( total_raised = sum(contribution_receipt_amount))



#returns tigris query file as a shapefile (if not, it's a weird list that doesn't join)
options(tigris_class = "sf")
#Download a Zip Code Tabulation Area (ZCTA) shapefile into R
#read in our shapefile
#run this so it remembers the map once you've downloaded 
options(tigris_use_cache = TRUE)
#code_shapefile <- zctas(cb = FALSE, year = 2010, state = "NC")
#code_shapefile <- zctas(cb = FALSE, year = 2010, state = "AK")
#code_shapefile <- zctas(cb = FALSE, year = 2010, state = "CO")
#code_shapefile <- zctas(cb = FALSE, year = 2010, state = "AZ")
#this gets the counties shapefile for the state of Arizona. IT's a big state. But only has 15 counties.
code_shapefile <- counties("Arizona", cb = TRUE)

#join democratic fundraising numbers with shapefile
#make sure the zip column is numeric and then join the two, so we can make the shapefile and contributions data work together like happy friends in leaflet
cong_tot <- left_join(code_shapefile, totals , by = c("NAME"= "county_name" ))
#and there's two counties that have a total of 0 (their populations are a total of 27k residents), so we're going to replace those values with zeroes.We'll create a new column just in case
cong_tot <- cong_tot %>% 
  mutate(total_raised_no_na = replace_na(total_raised, 0))

#########works, but is so slow because of all the shapefiles to plot.

ggplot(cong_tot) + 
  geom_sf(data = cong_tot) +
  aes(fill= cut(total_raised_no_na,breaks = c(-1, 1000, 5000, 20000, 100000, 800000), labels= c("0-1k", "1k-5k", "5k-20k", "20k-100k", "100k-800k") )) + 
  geom_sf(color="black")  +
  scale_fill_manual(values = c("#efeef6", "#C9C5DB", "#938CB8", "#3E386D", "#574e97")) +
  theme_void() + 
  labs(title="Funds raised in AZ counties", caption="Source: Federal Elections Commission", color='legend', fill='legend title')


#leaflet works much faster and it gives us a more interactive graphic, which i'm partial to.

bins <- c(0, 1000, 5000, 20000, 100000, 500000, Inf)
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






#helpful for running code in a smaller subset of data for testing purposes
#dt <- 
  #df %>%
  #slices off number of rows from 1:xx in df
  #slice(1:4)
  #slices off random number of rows from data
#  sample_n(mcsally, 100)


#from zip code and noncensus packages. Was used in original code. And Supposedly cleans up zip codes, gets me FIPS codes. Cran has been rudely pulled down the orphan packages which I found rude. It's useful. But if it's gone, it's gone. 
#library(zipcode)
#library(noncensus)
#senate$contributor_zip <- clean.zipcodes(senate$contributor_zip)
#conjures up the zip_codes table from the noncensus library. Which is a great resource for this project. Because, unlike the zip codes data(zipcodes) table. It has fips codes.
#data(zip_codes)
#however, there was a lack of zeroes in the fips code fields. And since the clean.zipcodes puts zeroes in front of some four-digit I'm using it here to match up the Census database.
#zip_codes$fips <- clean.zipcodes(zip_codes$fips)


#Gets aggregate data for candidates in a race.... not quite as clean

#office is senate, election year is 2020, candidate must have rised funds, The status of the candidte is "current" and i want to see the 2018 fiundraiosing cycle
#senate_search <- search_candidates(state = "AZ", office = "S", has_raised_funds = TRUE, candidate_status = "C", election_year = 2020) %>%
  #find their primary committee, a
# get_candidate_totals(designation = "P") 



######################


#USEFUL DOCUMENTATION #
#https://s3.amazonaws.com/ire16/campaign-finance/MiningFECData.pdf
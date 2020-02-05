library(noncensus)
library(tidyverse)
library(lubridate)
library(tigris)
library(stringr)
library(sf)
#for scale_fill_virdis
library(viridis)
library(leaflet)
library(zipcode)



#you can be inclusive and count for both, and in methodology, just say you counted some twice. Crosswalk


#df <- read_csv("presidential_jul_16.csv")
df <- read_csv("fec_data_oct.csv")

urban_rural <- read_csv("rural_urban_2013.csv")
#the goal of this analysis is to evaluate where, and how much, money is being donated to each candidate running for president


#Rural/urban codes were downloaded from https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx
#fec data accessed from https://classic.fec.gov/disclosurep/PDownload.do
#subsets the first 5 numbers of the zipcodes row https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/substr
df$contbr_zip <- substr(df$contbr_zip, 1, 5)
#FYI, if you want the last 5, use https://www.rdocumentation.org/packages/FedData/versions/1.1.0/topics/substrRight
#from zip code package. Supposedly cleans up zip codes.
df$contbr_zip <- clean.zipcodes(df$contbr_zip)
#conjures up the zip_codes table from the noncensus library. Which is a great resource for this project. Because, unlike the zip codes data(zipcodes) table. It has fips codes.
data(zip_codes)
#however, there was a lack of zeroes in the fips code fields. And since the clean.zipcodes puts zeroes in front of some four-digit I'm using it here to match up the Census database.
zip_codes$fips <- clean.zipcodes(zip_codes$fips)




#going to try and use the janitor package to find out about these crosswalk zipcodes that have several zipcodes that straddle congressional districts...I'm going to try and identify how many are in several places, and whether it's feasible to select the strongest correlator. 
#install.packages("janitor")
library(janitor)
# For the crosswalk https://www.huduser.gov/portal/datasets/usps_crosswalk.html // downloaded Oct 9. 2019
xwalk <- read_csv("ZIP_CD.csv")

#return me a df with just duplicates so I can check them out and see if there's a bunch of mooney in it 
#using the janitor library, I'm able to make a dataframe of the duplicates
#dupes <- xwalk %>% get_dupes(zip)
#still_dupes <- nodupes %>%
#  ungroup(zip) %>% 
#  get_dupes(zip)
#if things don't work quite right, when it comes to getting rid of duplicates, you have to use the ungroup() function to look at the column you did the duplicate analysis on...not sure wy this is the case. But I was getting an error and now it's working as a query. 
#Wow. so there are several districts that are exactly .5 So I'm going to just manually delete one of those
#checking <- left_join(still_dupes, zip_codes, by = c( "zip" = "zip"))

#so what I'd like to do is just choose, based on the total ratio of businesses and residences 
# I initially tried to do with just residential ratio. But some districts had no residential, same with business. And so I decided to opt for tot.
#nodupes <- xwalk %>% 
#  group_by(zip) %>%
#  top_n(1, abs(tot_ratio))


#props to Dhmontgomery in newsnerdery. to helping me eliminate the excess dupes by using a combination of rank and filter...basically what top_n does. But with the added ability of adding the ties.method element. #I was able to select the highest ratios and then, in the case of ties, R randomly chose one. 
nodupes <- xwalk %>% 
group_by(zip) %>%
  mutate(rank = rank(tot_ratio, ties.method = "random")) %>% 
  filter(rank < 2)

#I've used the nodupes for my join. But I will have to get a tiebreak from a data exper who can provide insight on which I should use for my final analysis.

#And finally we can join this behemouth together, to get a combo of donors and districts

districts_and_zips <- left_join(df, nodupes, by = c("contbr_zip" = "zip"))

# we may want to filter out the dates we want now, so we need to format 
districts_and_zips <- mutate(districts_and_zips, fixeddate = as.Date(contb_receipt_dt, "%d-%b-%y"))

#and let's give each area a classification ---- this was when I planned to organize each donation into rural and urban. I have decided to opt for not doing that

#These few pieces of code were important when I was comparing rural and urban. It is now not needed. But I'm leaving it in because I'm a hoarder
#doing the same treatment for the census table...Have consulted both and cross-checked about 20 fields to make sure they're correct. Also preserves the lines that begin with 0 as 0 
urban_rural$FIPS <- clean.zipcodes(urban_rural$FIPS)


#urban_rural$FIPS <- as.numeric(urban_rural$FIPS)
#now joins the two new tables I created with the urban rural table. !!!!
#census_and_zips <- left_join(new_data, urban_rural, by = c("fips" = "FIPS"))
#and I'll join again, so I can have the congressional districts as part of my total information package. So I have both urban/rural and congressional district. I'll probablty split it off later but since I have the data I'll use it. Now, I need the xwalk file zips on the xwalk file...I will also filter the results based on the tot ratio
#districts_and_zips <- mutate(districts_and_zips , type = if_else(RUCC_2013 == 1, "large urban", if_else(RUCC_2013 == 2 | RUCC_2013 == 3, "urban under 1 million", (if_else(RUCC_2013 == 4 |RUCC_2013 == 6 | RUCC_2013 == 8  , "adjacent to metro" , "not adjacent to metro")))))

#Check the Nas... put them in a separate df and let's see what zips are NAs.
#nas <- districts_and_zips %>%
#  filter(is.na(state)) %>%
#  group_by(contbr_zip) %>%
#  summarise(total_raised = sum(contb_receipt_amt ))
#Let's see what the total amount of the NAs is... OK about 1 million 1,048,393
#sum(nas$total_raised)
#how much is the total amount in this spreadsheet wow, lots more. 269,581,527 So total nas is about .3%
#sum(districts_and_zips$contb_receipt_amt)
#is the number of NAs consistent?
#sum(is.na(new_data$fips)) 
#check against df
#sum(is.na(df$contbr_zip)) 


#Check the Nas
#sum(is.na(census_and_zips$fips)) 



#let's filtr out a few problmeatic large-dollar transfers from committees that the FEC didn't filter out the !str_detect way of coding tells R select the opposite data what I'm asking you to filter out
#first let's see what rows we will be removing Looks like just what we wanted
#test <- census_and_zips %>% filter(str_detect(str_to_lower(contbr_nm), "unitemized"))
#ok now let's just 
new_districts_and_zips_1 <- districts_and_zips %>%
  filter(!str_detect(str_to_lower(contbr_nm), "unitemized"))



# QUERIES 
#join state names with the fips codes so we can understand what state is being mentioned 
#scraped and downloadwed from here https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696

abrevs <- read_csv("abbrevs.csv")
#so join the districts and states with the donations
new_districts_and_zips_1$state <- as.numeric(new_districts_and_zips_1$state)
new_districts_and_zips <- left_join(new_districts_and_zips_1, abrevs, by = c( "state" = "FIPS" ))
#now that we've got evrerything together we need to create a column that has the state and the district together. 
new_districts_and_zips <- new_districts_and_zips %>%
  mutate(cong_dist = (paste(`Postal Code`, "-", district)))
#what if we just looked at the democrats raising money from different districts. Will filter out Donald J. Trump, so we don't get some weird results
#gives us a not in function. Instead of having to use !=
`%not_in%` <- negate(`%in%`)

#which district spent the most money supporting democratic candidates?
dist_tot <- new_districts_and_zips %>%
    group_by(state, district, Name, cong_dist) %>%
    filter( cand_nm %not_in% "Trump, Donald J.") %>%
    summarise(total_raised = sum(contb_receipt_amt )) %>%
    arrange(desc(total_raised))

#which districts spend the most money supporting trump?
trump_tot <- new_districts_and_zips %>%
  group_by(state, district, Name, cong_dist) %>%
  filter( cand_nm %in% "Trump, Donald J.") %>%
  summarise(total_raised = sum(contb_receipt_amt )) %>%
  arrange(desc(total_raised))
 

#Which districts supported each individual candiate the most? Date filter is noted. Can be re-activated, if you want.  
cand_total <- new_districts_and_zips %>%
  #  filter(fixeddate >= as.Date("2019-04-01")) %>%
  group_by(cand_nm, cong_dist) %>%
  summarise(total_raised = sum(contb_receipt_amt))

#select the top 6 from each candidate
top_dist_candidate <- cand_total %>% 
  group_by(cand_nm) %>%
  top_n(10, abs(total_raised))

#Nathan Gonzales has a list of ratings for races. And I'm filtering out just the ones rated as "toss-up" or tilt dem/rep https://insideelections.com/ratings/house
#To understand who those voters are supporting could give us more information about what types of candidates are likely to be more popular by these voters in the general. And give me that info sicne Jan 1.
#remove the white space to make sure it's matching.
new_districts_and_zips$cong_dist <- trimws(new_districts_and_zips$cong_dist)
toss_up <- new_districts_and_zips %>%
  filter( cong_dist %in% c('GA - 07', 'IA - 02', 'IL - 14', 'NC - 09', 'NY - 11', 'NY - 22', 'OK - 05', 'SC - 01', 'UT - 04', 'CA - 21','GA - 06','IA - 01','ME - 02','MI - 08','NJ - 02','NJ - 03','NM - 02','NY - 19','VA - 02','VA - 07','IL - 13','PA - 01','PA - 10','TX - 22','TX - 24') & (fixeddate >= as.Date("2019-01-01")))


#get the total amount raised by each candidate in those districts since Jan 1
candidate_breakdown <- toss_up %>%
  group_by(cand_nm, cong_dist) %>%
  summarise(dist_total = sum(contb_receipt_amt))
#give me the total each candidate raised since Apr. 1. 
total_raised <- new_districts_and_zips %>%
  filter(fixeddate >= as.Date("2019-01-01")) %>%
  group_by(cand_nm) %>%
  summarise(total_raised = sum(contb_receipt_amt)) 
#join the total raised by candidate with the total raised from specific districts. Yeahhhh, I know. it's a clunky way to do it. 
dist_totals <-  left_join(candidate_breakdown, total_raised, by = c ("cand_nm" = "cand_nm" ))
#create a row that gives the percent of total contributions earned by the campaign
dist_totals <- dist_totals %>%
  transform(pct_of_tot = (dist_total/total_raised)*100) %>%
  mutate_at(vars(matches("pct_of_tot")), round, 4)
 
  #now give me the top five earners as a percent of their total earnings for each congressional district
  percent_high <- dist_totals %>%  
  group_by(cong_dist) %>%
  top_n(5, abs(pct_of_tot))
  #now give me the top five earners total earnings for each congressional district
  total_high <- dist_totals %>%  
    group_by(cong_dist) %>%
    top_n(5, abs(dist_total))
  
#write_csv(total_high, "totalhigh.csv")
  #let's look at all candidates not named donald trump. And then donald trump. And how much they have earned. 
  #which district spent the most money supporting democratic candidates?
  not_trump <- dist_totals %>%
    group_by(cong_dist) %>%
    filter( cand_nm %not_in% c("Trump, Donald J.","Weld, William Floyd (Bill)","Walsh, Joe")) %>%
    summarise(total_not_trump = sum(dist_total)) 
  #which districts spend the most money supporting trump?
  is_trump <- dist_totals %>%
    group_by(cong_dist) %>%
    filter( cand_nm %in% "Trump, Donald J.") %>%
    summarise(total_trump = sum(dist_total)) 
  #make it into one frame so I can export to excel. 
  trump_not_trump <-  left_join(not_trump, is_trump, by = "cong_dist" )

  #now give me the difference in a new column so I can eyeball it
  trump_not_trump <- trump_not_trump %>%
  transform(difference = (total_not_trump - total_trump))
  write_csv(trump_not_trump, "trump_not_trump.csv")
  
  
  
  more <- dist_totals %>%
    group_by(cand_nm) %>%
    summarise(sum = sum(dist_total))

  
#remove scientific notation  
options(scipen=999)
#graphed from guide found here https://www.biostars.org/p/362024/
trump_not_trump %>%
    gather("District", "Funds_Raised",-cong_dist) %>%
    ggplot(aes(cong_dist, Funds_Raised, fill = District)) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_y_continuous(name="Funds_Raised", limits=c(0, 800000)) +
    theme_bw()

############Just cool map stuff ################

#returns tigris query file as a shapefile 
options(tigris_class = "sf")
#makes sure the year of the data is 2018, giving us the 116 districts
cd116 <- congressional_districts(cb = TRUE, year =  2018, resolution = '5m')
#pull fips data for states so we can get the names, and drop the counties, and stuff. We don't need that
fips <- fips_codes %>%
  select(state, state_name, state_code) %>%
  distinct(state, state_name, state_code)

#join democratic fundraising numbers with shapefile
cd116$STATEFP <- as.numeric(cd116$STATEFP)
cong_tot <- left_join(cd116, dist_tot, by = c("STATEFP" = "state", "CD116FP" = "district"))
#join trump fundraising numbers with shapefile 
cong_trump_tot <- left_join(cd116, trump_tot, by = c("STATEFP" = "state", "CD116FP" = "district"))


#http://strimas.com/r/tidy-sf/ help with analysis in sf
#read in our shapefile



#graphs the shapefile on a leafletmap -- cool -- alaska is much darker because it's damn huge
#https://rstudio.github.io/leaflet/legends.html

#A helpful suite of tools for making maps https://rstudio.github.io/leaflet/colors.html
#This map allows people to click on the map. 
bins <- c(0, 20000, 40000, 60000, 80000, 100000, 500000, 750000, 1000000, 1500000, 2000000, 3000000, 4000000, Inf)
pal <- colorBin("viridis", domain = cong_tot$total_raised, bins = bins)
map <- leaflet(cong_tot) %>% addTiles()
state_popup <- paste0("<strong> District: </strong>", 
                      cong_tot$cong_dist, 
                      "<br><strong>Total Raised by all Democrats: </strong>", 
                      cong_tot$total_raised)
leaflet(data = cong_tot) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(total_raised), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", pal = pal, values = ~total_raised,
            title = "Total raised by Democrats",
            labFormat = labelFormat(prefix = " "))


#for trump fundraising items

pal1 <- colorBin("inferno", domain = cong_trump_tot$total_raised)
map <- leaflet(cong_trump_tot) %>% addTiles()
state_popup1 <- paste0("<strong> District: </strong>", 
                       cong_trump_tot$cong_dist, 
                      "<br><strong>Total Raised by Trump: </strong>", 
                      cong_trump_tot$total_raised)
leaflet(data = cong_trump_tot) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal1(total_raised), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup1) %>%
  addLegend("bottomright", pal = pal1, values = ~total_raised,
            title = "Total raised by Trump",
            labFormat = labelFormat(prefix = " "))

##############
# average total fundraising for  // using ggplot //which was hard to read with the given country's size 
# plot
ggplot(cong_trump_tot) +
  geom_sf(aes(fill = total_raised)) +
  scale_fill_distiller("Area", palette = "Greens") +
  ggtitle("Mean area by group") +
  theme_bw()
##################


#helpful testing tool below 
#pulls random sampling of 1,000 rows creates a smaller chunk of data for code testing purposes
dt <- 
  #df %>%
  #slices off number of rows from 1:xx in df
  #slice(1:4)
  #slices off random number of rows from data
  sample_n(df, 1000)






#Queries I didn't use 




#OK let's do a couple of analysis pieces Let's just look at the most recent donation data.
# filter is noted, but if you want, you can filter the dates for Q2/or Q3, and see what each candidate raised. 
name_type_raised <- new_census_and_zips %>%
  filter(fixeddate >= as.Date("2019-04-01")) %>%
  group_by(cand_nm, type) %>%
  summarise(total_raised = sum(contb_receipt_amt))


#now let's look at the percentage of the total raised by each candidate. used to get a nice percent sign and rounded number https://stackoverflow.com/questions/29549731/finding-percentage-in-a-sub-group-using-group-by-and-summarise/29549927
name_type_raised <- name_type_raised %>%
  group_by(cand_nm) %>%
#if you want a percent sign use this. But you cant' sort in your df, for anything that way.
#mutate(percent=paste0(round(total_raised/sum(total_raised)*100, 2), "%")) 
mutate(percent=(round(total_raised/sum(total_raised)*100, 2))) 



#Write the file, if needed.
#write_csv(name_type_raised, "name_type_raised.csv")
#write_csv(total_raised, "totalraised.csv")
#write_csv(cand_total, "cand_total.csv")
  
#quick look at which counties gave the most money...this includes Trump of the areas 
new_census_and_zips %>%
group_by(County_Name, State) %>%
filter( cand_nm != "Trump, Donald J.") %>%
summarise(total_raised = sum(contb_receipt_amt )) %>%
arrange(desc(total_raised))



#USING a shapeflie adn the SF package. 
#This file ended up being huge So I've decided to try using tigris for a smaller shapefile
cong <- st_read("tl_2018_us_cd116/tl_2018_us_cd116.shp")

#eliminates the scientific notation

options(scipen = 999)

name_type_raised %>%
  ggplot(aes(fill=type, y=percent, x=reorder(cand_nm, total_raised))) + 
    geom_bar(position="dodge", stat="identity") +
    coord_flip()
  
name_type_raised %>%
    filter(type == "large urban") %>%
    ggplot(aes(fill=type, y=percent, x=reorder(cand_nm, percent))) + 
    geom_bar(position="dodge", stat="identity") +
    coord_flip()
  
  
  ggplot(total_raised, aes(x=reorder(type, -percent), y=percent)) + 
    geom_bar(position="dodge", stat="identity")
  
#last step. We need to categorize each district. 


sum(new_districts_and_zips$contb_receipt_amt)




#look at the number of donors
#how many of those donors are new
#look at previous cycles for context and see if the results correlated with trends
#look at the zip codes within a given district

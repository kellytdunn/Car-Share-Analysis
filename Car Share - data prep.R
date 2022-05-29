install.packages("mapproj")
library(ggplot2)
library(dplyr)

setwd("C:/Users/kelly/OneDrive/Documents/Stats with R Certificate/Quarter 2/HW/Final Project")
fulldata = read.csv("data/household.csv") #Household Travel Survey from PSRC


#narrow down to the variables I want to use
surveydata = subset(fulldata, select = c(car_share, numchildren, hh_wt_combined, cityofseattle, final_home_uvnum, hhsize, numworkers, hhincome_detailed, vehicle_count, offpark_cost, offpark, streetpark))

summary(surveydata)
rm(fulldata)
table(surveydata$final_home_uvnum)


#add continuous income variable using midpoint using distribution as a guide. Make midpoints off center to reflect where the mean within the group is likely to fall. 
surveydata$hhincome_num = surveydata$hhincome_detailed #first make a copy to modify
surveydata$hhincome_num = replace(surveydata$hhincome_num, surveydata$hhincome_num == "Under $10,000", 8000)
surveydata$hhincome_num = replace(surveydata$hhincome_num, surveydata$hhincome_num == "$10,000-$24,999", 19000)
surveydata$hhincome_num = replace(surveydata$hhincome_num, surveydata$hhincome_num == "$25,000-$34,999", 32000)
surveydata$hhincome_num = replace(surveydata$hhincome_num, surveydata$hhincome_num == "$35,000-$49,999", 45000)
surveydata$hhincome_num = replace(surveydata$hhincome_num, surveydata$hhincome_num == "$50,000-$74,999", 60000)
surveydata$hhincome_num = replace(surveydata$hhincome_num, surveydata$hhincome_num == "$75,000-$99,999", 85000)
surveydata$hhincome_num = replace(surveydata$hhincome_num, surveydata$hhincome_num == "$100,000-$149,999", 120000)
surveydata$hhincome_num = replace(surveydata$hhincome_num, surveydata$hhincome_num == "$150,000-$199,999", 170000)
surveydata$hhincome_num = replace(surveydata$hhincome_num, surveydata$hhincome_num == "$200,000-$249,999", 220000)
surveydata$hhincome_num = replace(surveydata$hhincome_num, surveydata$hhincome_num == "$250,000 or more", 260000)

#take a closer look at the rows with no income listed. 
incomeblank = surveydata[surveydata$hhincome_num == "Prefer not to answer",]


#drop NAs from hhincome_num. They don't seem noticeably different from the others and they are only 10% of observations so I don't want to drop the whole variable. 
surveydata = surveydata[surveydata$hhincome_num != "Prefer not to answer",]

rm(incomeblank)

table(surveydata$hhincome_num)


#convert data types and rename binned hhincome_detailed variable
surveydata$hhincome_bins = as.factor(surveydata$hhincome_detailed)
surveydata$hhincome_num = as.integer(surveydata$hhincome_num)
surveydata$urban_village_name = as.factor(surveydata$final_home_uvnum)

#recode blanks in urban_village_name
surveydata$urban_village_name = replace(surveydata$urban_village_name, surveydata$urban_village_name == "", "(None)")
surveydata = droplevels(surveydata) #drop unused factor "" in urban_village_name

#new variable to categorize urban villages by type. otherwise it's too many to deal with. 
surveydata$urban_village_type = surveydata$urban_village_name

col <- c("urban_village_type")



surveydata$urban_village_type = dplyr::recode(surveydata$urban_village_type,
`(None)` = "No UV",                        `Madison-Miller`  = "Residential",                   `Pike/Pine` = "Urban Center",                        
`First Hill` = "Urban Center",                        `Belltown` = "Urban Center",                        `Roosevelt`  = "Residential",                       
`Fremont`= "Hub",                          `South Lake Union` = "Urban Center",                 `Capitol Hill` = "Urban Center",                    
`Northgate` = "Urban Center",           `Ballard-Interbay-Northend` = "Industrial",       `Lake City` = "Hub",                      
`University District Northwest` = "Urban Center",    `Uptown` = "Urban Center",                           `Commercial Core` = "Urban Center",                 
`23rd & Union-Jackson` = "Residential",            `Greater Duwamish` = "Industrial",                `Eastlake` = "Residential",                         
`Wallingford` = "Residential",                       `Admiral` = "Residential",                           `Bitter Lake Village` = "Hub",            
`Green Lake` = "Residential",                        `Upper Queen Anne`  = "Residential",                 `12th Avenue` = "Urban Center",                     
`Chinatown-International District`= "Urban Center",  `Crown Hill`  = "Residential",                       `Ravenna`  = "Residential",                         
`Denny Triangle`= "Urban Center",                   `Greenwood-Phinney Ridge`  = "Residential",          `Columbia City` = "Residential",                  
`Ballard` = "Hub",                          `Pioneer Square` = "Urban Center",                 `West Seattle Junction`= "Hub",          
`Aurora-Licton Springs` = "Residential",             `North Beacon Hill` = "Residential",                 `North Rainier` = "Hub",                  
`South Park`= "Industrial",                       `University Campus`= "Urban Center",                 `Othello`  = "Residential",                         
`Rainier Beach`  = "Residential",                    `Morgan Junction`  = "Residential",                  `Westwood-Highland Park` = "Residential")   



table(surveydata$hhincome_bins)

#shorten hhincome_bins labels

surveydata$hhincome_bins = dplyr::recode(surveydata$hhincome_bins,
`Under $10,000` = "Under 10k",             
`$10,000-$24,999` = "10 - 25k", 
`$25,000-$34,999` = "25 - 35k", 
`$35,000-$49,999` = "35 - 50k",
`$50,000-$74,999` = "50 - 75k", 
`$75,000-$99,999` = "75 - 100k", 
`$100,000-$149,999` = "100 - 150k", 
`$150,000-$199,999` = "150 - 200k",
`$200,000-$249,999` = "200 - 250k", 
`$250,000 or more` = "Over 250k")

#reorder hhincome_bins 
incomeorder = c("Under 10k","10 - 25k", "25 - 35k", "35 - 49k", "50 - 75k",
                "75 - 100k", "100 - 150k", "150 - 200k", "200 - 250k", "Over 250k")
surveydata$hhincome_bins <- factor(surveydata$hhincome_bins, levels = incomeorder) 

ggplot(surveydata[surveydata$urban_village_type != "Industrial",], aes(x = hhincome_bins, fill = factor(car_share))) + geom_bar(position = "fill") +
  labs(x='Household Income', y='Proportion with Car Share') + guides(fill='none') +
  scale_fill_manual(values = c('white','black')) + theme(axis.text.x = element_text(angle = 45, size = 10, vjust = 0.5, hjust=0.5)) +
  facet_grid(.~urban_village_type) + scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

table(surveydata$hhincome_bins)


#check distribution of income
ggplot(surveydata, aes(x = hhincome_bins)) + geom_histogram(stat = "count") +
  labs(title="Income", x ="Income Bin", y = "Number of Households") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

table(surveydata$hhincome_num)

#make vehicle_count an integer
surveydata$vehicle_count = replace(surveydata$vehicle_count, surveydata$vehicle_count == "0 (no vehicles)", "0")
surveydata$vehicle_count = replace(surveydata$vehicle_count, surveydata$vehicle_count == "10 or more vehicles", "10")
surveydata$vehicle_count = as.integer(surveydata$vehicle_count)


table(surveydata$cityofseattle) #maybe should exclude people not in city, since people in city are oversampled and I'm not sure how to apply survey weights in R.
#I think I'm still justified in removing it though, and keeping final_uvnum. 

table(surveydata$car_share)
summary(surveydata)

#make car_share a binary variable
surveydata$car_share = replace(surveydata$car_share, surveydata$car_share == "No", "0")
surveydata$car_share = replace(surveydata$car_share, surveydata$car_share== "Yes", "1")
surveydata$car_share = as.integer(surveydata$car_share)

#bin numworkers because too few HHs have 4 or 5 workers.
table(surveydata$numworkers) 
surveydata$numworkers_bin = as.character(surveydata$numworkers)
surveydata$numworkers_bin = replace(surveydata$numworkers_bin, surveydata$numworkers_bin == "5", "3 or more")
surveydata$numworkers_bin = replace(surveydata$numworkers_bin, surveydata$numworkers_bin == "4", "3 or more")
surveydata$numworkers_bin = replace(surveydata$numworkers_bin, surveydata$numworkers_bin == "3", "3 or more")
surveydata$numworkers_bin = as.factor(surveydata$numworkers_bin)
table(surveydata$numworkers_bin)


#new variable: vehicles per worker
surveydata$veh_per_worker = surveydata$vehicle_count/surveydata$numworkers
table(surveydata$veh_per_worker)

#but this allows infinite values, so need to adjust. 
surveydata$veh_per_worker = replace(surveydata$veh_per_worker, surveydata$veh_per_worker == "Inf", 0)

table(surveydata$streetpark)
#many blank values that did not show up as NAs. Delete variable. 

library(GGally)
testdata = subset(surveydata, select = -c(urban_village_name))



#hhsize correlates to much with numchildren. numworkers is a better measure anyway. 
#will need to remove offpark and offpark_cost unless I can figure out a reason there were so many NAs.
#also remove cityofseattle because it correlates highly with urban_village_name.

surveydata = subset(surveydata, select = -c(offpark, offpark_cost, cityofseattle, hhincome_detailed, hhsize, streetpark, final_home_uvnum))

#now look at distributions and see if we need transformations.

hist(surveydata$numchildren) #this is surprising.
hist(incomeblank$numchildren)


hist(surveydata$vehicle_count)
hist(incomeblank$vehicle_count)


#transform hhincome_num to the thousands.
surveydata$hhincome_num = surveydata$hhincome_num/1000

#plan to center income bins during modeling; not doing it here because plots would be harder to interpret. 

#exploring.

ggplot(surveydata, aes(x = numchildren, fill = factor(car_share))) + geom_bar(position = "fill") +
  labs(x='Number of Children', y='Proportion with Car Share') + guides(fill='none') +
  scale_fill_manual(values = c('white','black'))

ggplot(surveydata, aes(x = urban_village_name, fill = factor(car_share))) + geom_bar(position = "fill") +
  labs(x='Urban Village', y='Proportion with Car Share') + guides(fill='none') +
  scale_fill_manual(values = c('white','black')) +  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

ggplot(surveydata, aes(x = hhincome_bins, fill = factor(car_share))) + geom_bar(position = "fill") +
  labs(x='Income', y='Proportion with Car Share') + guides(fill='none') +
  scale_fill_manual(values = c('white','black')) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggplot(surveydata, aes(x = vehicle_count, fill = factor(car_share))) + geom_bar(position = "fill") +
  labs(x='Number of Vehicles in HH', y='Proportion with Car Share') + guides(fill='none') +
  scale_fill_manual(values = c('white','black')) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

ggplot(surveydata, aes(x = numchildren, fill = factor(car_share))) + geom_bar(position = "fill") +
  labs(x='Number of Children', y='Proportion with Car Share') + guides(fill='none') +
  scale_fill_manual(values = c('white','black')) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

ggplot(surveydata, aes(x = numworkers_bin, fill = factor(car_share))) + geom_bar(position = "fill") +
  labs(x='Number of Workers', y='Proportion with Car Share') + guides(fill='none') +
  scale_fill_manual(values = c('white','black')) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

plot(tapply(surveydata$vehicle_count, INDEX = surveydata$hhincome_bins, FUN = mean))

#pct car share by income bin:
car_share_totals <- surveydata %>% group_by(hhincome_bins)  %>%
  summarize( n=n(), pct_car_share= 100*mean(car_share))

car_share_totals

rm(testdata)

#plots showing all relevant variables. 

ggplot(surveydata, aes(x = vehicle_count, fill = factor(car_share))) + geom_bar(position = "fill") +
  labs(x='Vehicle Count', y='Proportion with Car Share') + guides(fill='none') +
  scale_fill_manual(values = c('white','black')) +
  facet_grid(.~hhincome_bins, labeller = labeller(hhincome_bins = label_wrap_gen(width = 7))) + scale_x_continuous(breaks=c(0, 2, 4, 6, 8)) + theme_bw() +
  theme(panel.grid.major = element_blank())
#good one that shows that vehicle count matters more for higher incomes. 

ggplot(surveydata[surveydata$urban_village_type != "Industrial",], aes(x = hhincome_bins, fill = factor(car_share))) + geom_bar(position = "fill") +
  labs(x='Household Income', y='Proportion with Car Share') + guides(fill='none') +
  scale_fill_manual(values = c('white','black')) + theme(axis.text.x = element_text(angle = 45, vjust = 0.2, hjust=0.2)) +
  facet_grid(.~urban_village_type) + scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
#another good one that shows that income matters more depending on UV type. 



table(surveydata$urban_village_type, surveydata$hhincome_bins)


save.image('Prep_workspace.RData') #save workspace

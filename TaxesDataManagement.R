rm(list = ls())

library(plyr)

setwd("C://Users//Sheryl//Documents//PSC 631 Adv. Stats//State Taxes")

boda <- read.csv("BODAcious.9.6.14.csv")
govelecs <- read.csv("StateElections_Gub_2012_09_06_Public_Version.csv")
partbalance <- read.csv("Partisan_Balance_For_Use2011_06_09b-1.csv")
stateideo <- read.csv("shor-mccarty1993-2013stateagg.csv")
stateecon <- read.csv("State_Econ_Quarterly2012_09_06.csv")

boda$state <- as.character(boda$state)
boda$state[boda$state == "Massachusettes"] <- "Massachusetts"

# Creating joins for year and state
boda$JoinYear <- boda$FY
boda$JoinState <- boda$state

govelecs$JoinYear <- govelecs$year
govelecs$JoinState <- govelecs$state

partbalance$JoinYear <- partbalance$year
partbalance$JoinState <- partbalance$state

# Get state ideology state variable in correct format 

stateideo$state[stateideo$fips == 1] <- "Alabama"
stateideo$state[stateideo$fips == 2] <- "Alaska"
stateideo$state[stateideo$fips == 4] <- "Arizona"
stateideo$state[stateideo$fips == 5] <- "Arkansas"
stateideo$state[stateideo$fips == 6] <- "California"
stateideo$state[stateideo$fips == 8] <- "Colorado"
stateideo$state[stateideo$fips == 9] <- "Connecticut"
stateideo$state[stateideo$fips == 10] <- "Delaware"
stateideo$state[stateideo$fips == 12] <- "Florida"
stateideo$state[stateideo$fips == 13] <- "Georgia"
stateideo$state[stateideo$fips == 15] <- "Hawaii"
stateideo$state[stateideo$fips == 16] <- "Idaho"
stateideo$state[stateideo$fips == 17] <- "Illinois"
stateideo$state[stateideo$fips == 18] <- "Indiana"
stateideo$state[stateideo$fips == 19] <- "Iowa"
stateideo$state[stateideo$fips == 20] <- "Kansas"
stateideo$state[stateideo$fips == 21] <- "Kentucky"
stateideo$state[stateideo$fips == 22] <- "Louisiana"
stateideo$state[stateideo$fips == 23] <- "Maine"
stateideo$state[stateideo$fips == 24] <- "Maryland"
stateideo$state[stateideo$fips == 25] <- "Massachusetts"
stateideo$state[stateideo$fips == 26] <- "Michigan"
stateideo$state[stateideo$fips == 27] <- "Minnesota"
stateideo$state[stateideo$fips == 28] <- "Mississippi"
stateideo$state[stateideo$fips == 29] <- "Missouri"
stateideo$state[stateideo$fips == 30] <- "Montana"
stateideo$state[stateideo$fips == 31] <- "Nebraska"
stateideo$state[stateideo$fips == 32] <- "Nevada"
stateideo$state[stateideo$fips == 33] <- "New Hampshire"
stateideo$state[stateideo$fips == 34] <- "New Jersey"
stateideo$state[stateideo$fips == 35] <- "New Mexico"
stateideo$state[stateideo$fips == 36] <- "New York"
stateideo$state[stateideo$fips == 37] <- "North Carolina"
stateideo$state[stateideo$fips == 38] <- "North Dakota"
stateideo$state[stateideo$fips == 39] <- "Ohio"
stateideo$state[stateideo$fips == 40] <- "Oklahoma"
stateideo$state[stateideo$fips == 41] <- "Oregon"
stateideo$state[stateideo$fips == 42] <- "Pennsylvania"
stateideo$state[stateideo$fips == 44] <- "Rhode Island"
stateideo$state[stateideo$fips == 45] <- "South Carolina"
stateideo$state[stateideo$fips == 46] <- "South Dakota"
stateideo$state[stateideo$fips == 47] <- "Tennessee"
stateideo$state[stateideo$fips == 48] <- "Texas"
stateideo$state[stateideo$fips == 49] <- "Utah"
stateideo$state[stateideo$fips == 50] <- "Vermont"
stateideo$state[stateideo$fips == 51] <- "Virginia"
stateideo$state[stateideo$fips == 53] <- "Washington"
stateideo$state[stateideo$fips == 54] <- "West Virginia"
stateideo$state[stateideo$fips == 55] <- "Wisconsin"
stateideo$state[stateideo$fips == 56] <- "Wyoming"

stateideo$JoinYear <- stateideo$year
stateideo$JoinState <- stateideo$state

# State Econ stateno into correct format

stateecon$state[stateecon$stateno == 1] <- "Alabama"
stateecon$state[stateecon$stateno == 2] <- "Alaska"
stateecon$state[stateecon$stateno == 3] <- "Arizona"
stateecon$state[stateecon$stateno == 4] <- "Arkansas"
stateecon$state[stateecon$stateno == 5] <- "California"
stateecon$state[stateecon$stateno == 6] <- "Colorado"
stateecon$state[stateecon$stateno == 7] <- "Connecticut"
stateecon$state[stateecon$stateno == 8] <- "Delaware"
stateecon$state[stateecon$stateno == 9] <- "Florida"
stateecon$state[stateecon$stateno == 10] <- "Georgia"
stateecon$state[stateecon$stateno == 11] <- "Hawaii"
stateecon$state[stateecon$stateno == 12] <- "Idaho"
stateecon$state[stateecon$stateno == 13] <- "Illinois"
stateecon$state[stateecon$stateno == 14] <- "Indiana"
stateecon$state[stateecon$stateno == 15] <- "Iowa"
stateecon$state[stateecon$stateno == 16] <- "Kansas"
stateecon$state[stateecon$stateno == 17] <- "Kentucky"
stateecon$state[stateecon$stateno == 18] <- "Louisiana"
stateecon$state[stateecon$stateno == 19] <- "Maine"
stateecon$state[stateecon$stateno == 20] <- "Maryland"
stateecon$state[stateecon$stateno == 21] <- "Massachusetts"
stateecon$state[stateecon$stateno == 22] <- "Michigan"
stateecon$state[stateecon$stateno == 23] <- "Minnesota"
stateecon$state[stateecon$stateno == 24] <- "Mississippi"
stateecon$state[stateecon$stateno == 25] <- "Missouri"
stateecon$state[stateecon$stateno == 26] <- "Montana"
stateecon$state[stateecon$stateno == 27] <- "Nebraska"
stateecon$state[stateecon$stateno == 28] <- "Nevada"
stateecon$state[stateecon$stateno == 29] <- "New Hampshire"
stateecon$state[stateecon$stateno == 30] <- "New Jersey"
stateecon$state[stateecon$stateno == 31] <- "New Mexico"
stateecon$state[stateecon$stateno == 32] <- "New York"
stateecon$state[stateecon$stateno == 33] <- "North Carolina"
stateecon$state[stateecon$stateno == 34] <- "North Dakota"
stateecon$state[stateecon$stateno == 35] <- "Ohio"
stateecon$state[stateecon$stateno == 36] <- "Oklahoma"
stateecon$state[stateecon$stateno == 37] <- "Oregon"
stateecon$state[stateecon$stateno == 38] <- "Pennsylvania"
stateecon$state[stateecon$stateno == 39] <- "Rhode Island"
stateecon$state[stateecon$stateno == 40] <- "South Carolina"
stateecon$state[stateecon$stateno == 41] <- "South Dakota"
stateecon$state[stateecon$stateno == 42] <- "Tennessee"
stateecon$state[stateecon$stateno == 43] <- "Texas"
stateecon$state[stateecon$stateno == 44] <- "Utah"
stateecon$state[stateecon$stateno == 45] <- "Vermont"
stateecon$state[stateecon$stateno == 46] <- "Virginia"
stateecon$state[stateecon$stateno == 47] <- "Washington"
stateecon$state[stateecon$stateno == 48] <- "West Virginia"
stateecon$state[stateecon$stateno == 49] <- "Wisconsin"
stateecon$state[stateecon$stateno == 50] <- "Wyoming"

abb.stateecon <- stateecon[stateecon$quar == 2.5,]

abb.stateecon$JoinYear <- abb.stateecon$year
abb.stateecon$JoinState <- abb.stateecon$state

# Join Data

data <- join(boda, govelecs, by = c("JoinYear", "JoinState")) 

data <- join(data, partbalance, by = c("JoinYear", "JoinState")) 

data <- join(data, stateideo, by = c("JoinYear", "JoinState")) 

data <- join(data, abb.stateecon, by = c("JoinYear", "JoinState"))

# Write to csv

write.csv(data, "StateTaxesData.csv")
## Reproducible Research # 2

require(data.table)
require(dplyr)
require(stringr)
require(xlsx)
require(ggplot2)
require(maps)
require(grid)
require(gridExtra)
require(R.cache)

## documentation of the data is here:
## https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf

############################################### DATA PROCESSING ################################################
## get the source file, read as binary
## the data have already been downloaded and should be in the current directory. If not,
## download the data here:
##setInternet2(use = TRUE)
##download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "FStormData.bz2", mode = "wb")

## it's not necessary to decompress the file, R can read it directly. See this link...
## https://entropicevolution.wordpress.com/2014/01/24/learning-r-r-can-read-compressed-files/

dt =  evalWithMemoization(read.table("FStormData.bz2", header = TRUE, fill = TRUE, stringsAsFactors = F, sep = ","))

## convert dates to a useful format
dt$BGN_DATE = as.Date(dt$BGN_DATE, "%m/%d/%Y")
dt$END_DATE = as.Date(dt$END_DATE, "%m/%d/%Y")

## it appears that event reporting increased substantially beginning in 1993.
## Since years before 1994 are inconsistent, events occurring before 1994 are ignored
dt = filter(dt, BGN_DATE >= "1994-01-01")

## let's also ignore events that caused no injuries, fatalities, property damage, and crop damage
harmful = filter(dt, INJURIES > 0 | FATALITIES > 0 | PROPDMG > 0 | CROPDMG > 0)

## there are 449 different levels of event type, many of which do not match
## the event categories in the National Weather Service Instructions 10-1605
## <https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf>
## The reported events should be collapsed into the National Weather Service (NWS) 
## categories listed in the documentation. We'll start by creating a NWS column 
## which indicates the new assigned NWS category
harmful$NWS = NA

## eventmap.xlsx contains a mapping of the reported events in the original data to the 
## event categories in the NWS Instructions 10-1605.
## The map was created by determining the closest match of the reported event in the data
## to the NWS category. In cases in which the event was listed as combinations (such as
## Hurricane/High Winds) the first reported event listed was used, on the assumption that
## the data collector listed the most significant event first.

## load the mapping file
map = read.xlsx2("eventmap.xlsx", 1, stringsAsFactors = FALSE)

## loop through the mapping file, adding the NWS-mapped event type to the harmful data 
for (i in 1:nrow(map)){
    harmful$NWS[which(harmful$EVTYPE == map$EVTYPE[i])] = map$NWS[i]
}

## make the NWS category a factor
harmful$NWS = as.factor(harmful$NWS)
    
################# prepare the data to calculate economic consequence
## the new PropMult column will be used as a numeric factor (based on the PROPDMGEXP column)
## with which to multiply the PROPDMG amount. The default value is 1
harmful$PropMult = 1
harmful$PropMult[which(harmful$PROPDMGEXP == 'b' | harmful$PROPDMGEXP == 'B')] = 1000000000
harmful$PropMult[which(harmful$PROPDMGEXP == 'm' | harmful$PROPDMGEXP == 'M')] = 1000000
harmful$PropMult[which(harmful$PROPDMGEXP == 'k' | harmful$PROPDMGEXP == 'K')] = 1000
harmful$PropMult[which(harmful$PROPDMGEXP == 'h' | harmful$PROPDMGEXP == 'H')] = 100
harmful$PropMult[which(harmful$PROPDMGEXP == '0') ] = 1
harmful$PropMult[which(harmful$PROPDMGEXP == '2') ] = 100
harmful$PropMult[which(harmful$PROPDMGEXP == '3') ] = 1000
harmful$PropMult[which(harmful$PROPDMGEXP == '4') ] = 10000
harmful$PropMult[which(harmful$PROPDMGEXP == '5') ] = 100000
harmful$PropMult[which(harmful$PROPDMGEXP == '6') ] = 1000000
harmful$PropMult[which(harmful$PROPDMGEXP == '7') ] = 10000000

## calculate the costs
harmful$PropCost = as.numeric(harmful$PROPDMG * harmful$PropMult)

## we need a similar transformation for the crop damage calculation. The CROPDMGEXP
## column has fewer unique values than the PROPDMGEXP column
harmful$CropMult = 1
harmful$CropMult[which(harmful$CROPDMGEXP == 'b' | harmful$CROPDMGEXP == 'B')] = 1000000000
harmful$CropMult[which(harmful$CROPDMGEXP == 'm' | harmful$CROPDMGEXP == 'M')] = 1000000
harmful$CropMult[which(harmful$CROPDMGEXP == 'k' | harmful$CROPDMGEXP == 'K')] = 1000
harmful$CropMult[which(harmful$CROPDMGEXP == 'h' | harmful$CROPDMGEXP == 'H')] = 100
harmful$CropMult[which(harmful$CROPDMGEXP == '0') ] = 1

## now calculate the costs
harmful$CropCost = as.numeric(harmful$CROPDMG * harmful$CropMult)

## for discussion purposes, summarize the overall damage by event type, over all areas
damage = harmful %>%
    group_by(NWS) %>%
    summarize(Fatalities = sum(FATALITIES), Injuries = sum(INJURIES), 
              Property = sum(PropCost), Crops = sum(CropCost), Count = n())

## determine the top 10 most harmful events in each category...
fatalities = damage %>% arrange(desc(Fatalities)) %>% select(NWS, Fatalities) %>% top_n(10, Fatalities) %>% rename("Event" = NWS)
injuries = damage %>% arrange(desc(Injuries)) %>% select(NWS, Injuries) %>% top_n(10, Injuries) %>% rename("Event" = NWS)
property = damage %>% arrange(desc(Property)) %>% top_n(10, Property) %>% select(NWS, Property) %>% rename("Event" = NWS, "Property Damage ($)" = Property) 
crops = damage %>% arrange(desc(Crops)) %>% top_n(10, Crops) %>% select(NWS, Crops) %>% rename("Event" = NWS, "Crop Damage ($)" = Crops) 
mostharmful = cbind(fatalities, injuries, property, crops)

## Now calculating the damage by event type for each state
damagebystate = harmful %>%
    group_by(STATE__, NWS) %>%
    summarize(Fatalities = sum(FATALITIES), Injuries = sum(INJURIES), 
              Property = sum(PropCost), Crops = sum(CropCost), Count = n())

## pull in the fips names here and merge with the damagebystate data
fips = read.xlsx("statefips.xlsx", 1, stringsAsFactors = FALSE)

## merge the fips info with the damagebystate. This will give us state names to go
## with all the damage
damagebystate = merge(damagebystate, fips, by.x = 'STATE__', by.y = 'FIPS', all.x = TRUE)

## for each state, determine the event leading to the maximum number of deaths
maxdeaths = damagebystate %>%
            group_by(AreaName) %>%
            filter(Fatalities == max(Fatalities)) %>%
            arrange(AreaName) %>%
            select(STATE__, AreaName, NWS, Fatalities)
maxdeaths$AreaName = tolower(maxdeaths$AreaName)

## for each state, determine the event leading to the maximum number of injuries
maxinjuries = damagebystate %>%
            group_by(AreaName) %>%
            filter(Injuries == max(Injuries)) %>%
            arrange(AreaName) %>%
            select(STATE__, AreaName, NWS, Injuries)
maxinjuries$AreaName = tolower(maxinjuries$AreaName)

## for each state, determine the event leading to the maximum property damage
maxpropdamage = damagebystate %>%
    group_by(AreaName) %>%
    filter(Property == max(Property)) %>%
    arrange(AreaName) %>%
    select(STATE__, AreaName, NWS, Property)
maxpropdamage$AreaName = tolower(maxpropdamage$AreaName)

## for each state, determine the event leading to the maximum crop damage
maxcropdamage = damagebystate %>%
    group_by(AreaName) %>%
    filter(Crops == max(Crops)) %>%
    arrange(AreaName) %>%
    select(STATE__, AreaName, NWS, Crops)
maxcropdamage$AreaName = tolower(maxcropdamage$AreaName)

## load us map info
states_map = map_data("state")

################## preparation for population health results
## join the states mapping data with the death data to map the death data
statesdeath = merge(states_map, maxdeaths, by.x = "region", by.y = "AreaName")
statesdeath$Event = as.factor(statesdeath$NWS)
statesinjury = merge(states_map, maxinjuries, by.x = "region", by.y = "AreaName")
statesinjury$Event = as.factor(statesinjury$NWS)

## remove unneeded columns and prepare in long format
statesdeath = statesdeath %>%
    select(long, lat, group, Event, Fatalities) %>%
    rename(value = Fatalities)
statesdeath$health = 'Fatalities'

statesinjury = statesinjury %>%
    select(long, lat, group, Event, Injuries) %>%
    rename(value = Injuries)
statesinjury$health = 'Injuries'

pophealth = rbind(statesdeath, statesinjury)

p1 = ggplot()
p1 = p1 + theme(axis.ticks = element_blank(), 
                axis.text.x = element_blank(), axis.text.y = element_blank(), 
                axis.title.x = element_blank(), axis.title.y = element_blank(),
                panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                panel.margin = unit(0.2, "lines"),
                panel.border = element_rect(color = "black", fill=NA, size=1))
p1 = p1 + geom_polygon(data = pophealth, aes(x=long, y=lat, group = group, fill = Event), color = "black") 
p1 = p1 + facet_wrap(~health,nrow=2)
p1

################### preparation for the economic consequences results
## join the states data with the property data
statesproperty = merge(states_map, maxpropdamage, by.x = "region", by.y = "AreaName")
statesproperty$Event = as.factor(statesproperty$NWS)
## join the states data with the injury data
statescrops = merge(states_map, maxcropdamage, by.x = "region", by.y = "AreaName")
statescrops$Event = as.factor(statescrops$NWS)

## remove unneeded columns and prepare in long format
statesproperty = statesproperty %>%
    select(long, lat, group, Event, Property) %>%
    rename(value = Property)
statesproperty$econ = 'Property'

statescrops = statescrops %>%
    select(long, lat, group, Event, Crops) %>%
    rename(value = Crops)
statescrops$econ = 'Crops'

economic = rbind(statesproperty, statescrops)

p2 = ggplot()
p2 = p2 + theme(axis.ticks = element_blank(), 
                axis.text.x = element_blank(), axis.text.y = element_blank(), 
                axis.title.x = element_blank(), axis.title.y = element_blank(),
                panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                panel.margin = unit(0.2, "lines"),
                panel.border = element_rect(color = "black", fill=NA, size=1))
p2 = p2 + geom_polygon(data = economic, aes(x=long, y=lat, group = group, fill = Event), color = "black") 
p2 = p2 + facet_wrap(~econ,nrow=2)
p2

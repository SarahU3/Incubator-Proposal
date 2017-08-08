setwd("~/GitHub/DataIncubator")

library(ggplot2)


#plot 1 - cost per watt of the top 5 solar states
sum(is.na(solarpv$state))
table(solarpv$state) #top 5: AZ, CA, MA, NJ, NY
topsolar <- subset(solarpv, state=="AZ" | state=="CA" | state=="MA" | state=="NJ" | state=="NY")

ggplot(topsolar, aes(x=topsolar$state, y=topsolar$cost_per_watt, fill=topsolar$state)) + geom_boxplot() + ylab("Cost per watt ($)") + xlab("State") +
  guides(fill=FALSE) + ggtitle("Top Five States - Solar Installation Costs") + theme_classic()

#plot 2 - size versus rebate
solarpv$numrebate <- as.numeric(solarpv$rebate)
residential <- subset(solarpv, install_type=="residential" & solarpv$annual_PV_prod<300000000)
ggplot(residential, aes(x=residential$size_kw, y=residential$numrebate)) + geom_point() + geom_smooth(colour="#2ca25f") + labs(x = "Size (KW)", y = "Rebate or Grant ($)", title="Size of installation and rebates", subtitle = "For residential installations") + theme_classic()

# mapping
library(maps)
library(maptools)
library(dplyr)
solarpv12_15 <- solarpv[which(solarpv$installyear>=2012 & solarpv$installyear<=2015),]
zipcodepv <- count(solarpv12_15, zipcode)
zipcodepv$zipfive <- sprintf("%05d", zipcodepv$zipcode)
statepv <- count(solarpv12_15, state)
names(statepv) <- c("state2", "value")
statepv$state <- state.name[match(statepv$state2,state.abb)]
statepv$state <- tolower(statepv$state)
statepv$state[statepv$state2=="DC"] <- "district of columbia"
###zipmap <- readShapeSpatial("~/GitHub/DataIncubator/2015_us_zcta/cb_2015_us_zcta510_500k.shp")
###statemap <- readShapeSpatial("~/GitHub/DataIncubator/2015_us_state/cb_2015_us_state_500k.shp")
###zipmapdf <- fortify(zipmap)
###statemapdf <- fortify(statemap)
statemap <- map_data("state")
ggplot(statepv, aes(map_id=state)) + geom_map(aes(fill = value), map = statemap) + 
  expand_limits(x = statemap$long, y = statemap$lat)
statepv$pct <- statepv$value/sum(statepv$value)*100
ggplot(statepv, aes(map_id=state)) + geom_map(aes(fill = pct), map = statemap) + 
  expand_limits(x = statemap$long, y = statemap$lat) +theme_bw() + 
  labs(title="Percent of solar installations by state", fill="Percent") + 
  scale_fill_gradient(low="gray90", high="green4")

##CAzipcodepv <- count(solarpv12_15, zipcode)
##zipcodepv$zipfive <- sprintf("%05d", zipcodepv$zipcode)

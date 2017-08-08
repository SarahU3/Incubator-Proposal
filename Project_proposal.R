setwd("~/GitHub/Incubator-Proposal")

library(ggplot2)


#plot 1 - cost per watt of the top 5 solar states
sum(is.na(solarpv$state))
table(solarpv$state) #top 5: AZ, CA, MA, NJ, NY
topsolar <- subset(solarpv, state=="AZ" | state=="CA" | state=="MA" | state=="NJ" | state=="NY")

ggplot(topsolar, aes(x=topsolar$state, y=topsolar$cost_per_watt, fill=topsolar$state)) + 
  geom_boxplot() + ylab("Cost per watt ($)") + xlab("State") +
  guides(fill=FALSE) + ggtitle("Top Five States - Solar Installation Costs") + theme_classic()

#plot 2 - size versus rebate
solarpv$numrebate <- as.numeric(solarpv$rebate)
residential <- subset(solarpv, install_type=="residential" & solarpv$annual_PV_prod<300000000)
ggplot(residential, aes(x=residential$size_kw, y=residential$numrebate)) + 
  geom_point() + geom_smooth(colour="#2ca25f") + 
  labs(x = "Size (KW)", y = "Rebate or Grant ($)", title="Size of installation and rebates", 
       subtitle = "For residential installations") + theme_classic()

#plot 3 - insolation in the top 5 solar states
ggplot(topsolar, aes(x=topsolar$state, y=topsolar$annual_insolation, fill=topsolar$state)) + 
  geom_boxplot() + ylab("Annual Insolation") + xlab("State") +
  guides(fill=FALSE) + ggtitle("Top Five States - Solar Insolation") + theme_classic()

#summary stats
solarpv$cost <- as.numeric(solarpv$cost)
solarpv$cost_per_watt <- as.numeric(solarpv$cost_per_watt)
solarpv$annual_PV_prod <- as.numeric(solarpv$annual_PV_prod)
solarpv$annual_insolation <- as.numeric(solarpv$annual_insolation)
solarpv$rebate <- as.numeric(solarpv$rebate)
solarpvsum <- solarpv %>% group_by(state) %>% summarise_at(vars(size_kw, cost, cost_per_watt, annual_PV_prod, annual_insolation, rebate), mean, na.rm=TRUE)


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
statepv <- merge.data.frame(x=statepv, y = solarpvsum, by.x = "state2", by.y = "state")
statepv$costratio <- statepv$rebate/statepv$cost
###zipmap <- readShapeSpatial("~/GitHub/Incubator-Proposal/2015_us_zcta/cb_2015_us_zcta510_500k.shp")
###statemap <- readShapeSpatial("~/GitHub/Incubator-Proposal/2015_us_state/cb_2015_us_state_500k.shp")
###zipmapdf <- fortify(zipmap)
###statemapdf <- fortify(statemap)
statemap <- map_data("state")
ggplot(statepv, aes(map_id=state)) + geom_map(aes(fill = value), map = statemap) + 
  expand_limits(x = statemap$long, y = statemap$lat)

statepv$pct <- (statepv$value/sum(statepv$value))*100
ggplot(statepv, aes(map_id=state)) + geom_map(aes(fill = pct), map = statemap) + 
  expand_limits(x = statemap$long, y = statemap$lat) +theme_bw() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  labs(title="Percent of solar installations by state", fill="Percent") + 
  scale_fill_gradient(low="gray90", high="green4")

ggplot(statepv, aes(map_id=state)) + geom_map(aes(fill = costratio), map = statemap) + 
  expand_limits(x = statemap$long, y = statemap$lat) +theme_bw() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  labs(title="Ratio of average rebate to average cost", fill="Rebate/cost") + 
  scale_fill_gradient(low="gray90", high="blue4")

##CAzipcodepv <- count(solarpv12_15, zipcode)
##zipcodepv$zipfive <- sprintf("%05d", zipcodepv$zipcode)

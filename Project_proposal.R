setwd("~/GitHub/DataIncubator")

library(ggplot2)
solar <- read.csv("~/GitHub/DataIncubator/openpv_all.csv")
#some basic data cleaning to collapse levels of factor solar$install_type
unique(solar$install_type)
levels(solar$install_type)[levels(solar$install_type)=="Residential"] <- "residential"
levels(solar$install_type)[levels(solar$install_type)==""] <- "unknown"
levels(solar$install_type)[levels(solar$install_type)=="Commerical"] <- "Commercial"
levels(solar$install_type)[levels(solar$install_type)=="Unknown"] <- "unknown"
levels(solar$install_type)[levels(solar$install_type)=="Commercial - Small Business"] <- "Commercial"
levels(solar$install_type)[levels(solar$install_type)=="Commercial - Agriculture"] <- "Commercial"
levels(solar$install_type)[levels(solar$install_type)=="Commercial - Builders"] <- "Commercial"
levels(solar$install_type)[levels(solar$install_type)=="Commercial - Other"] <- "Commercial"
levels(solar$install_type)[levels(solar$install_type)=="Small Business"] <- "Commercial"
levels(solar$install_type)[levels(solar$install_type)=="Agricultural"] <- "Commercial"
levels(solar$install_type)[levels(solar$install_type)=="agriculture"] <- "commercial"
levels(solar$install_type)[levels(solar$install_type)=="Commercial"] <- "commercial"
table(solar$install_type)

#plot 1 - cost per watt of the top 5 solar states
sum(is.na(solar$state))
table(solar$state) #top 5: AZ, CA, MA, NJ, NY
topsolar <- subset(solar, state=="AZ" | state=="CA" | state=="MA" | state=="NJ" | state=="NY")

ggplot(topsolar, aes(x=topsolar$state, y=topsolar$cost_per_watt, fill=topsolar$state)) + geom_boxplot() + ylab("Cost per watt ($)") + xlab("State") +
  guides(fill=FALSE) + ggtitle("Top Five States - Solar Installation Costs") + theme_classic()

#plot 2 - size versus rebate
solar$numrebate <- as.numeric(solar$rebate)
residential <- subset(solar, install_type=="residential" & solar$annual_PV_prod<300000000)
ggplot(residential, aes(x=residential$size_kw, y=residential$numrebate)) + geom_point() + geom_smooth(colour="#2ca25f") + labs(x = "Size (KW)", y = "Rebate or Grant ($)", title="Size of installation and rebates", subtitle = "For residential installations") + theme_classic()

library(plyr)
library(data.table)

storm <- read.csv("repdata-data-StormData.csv")
storm <- data.table(storm)

summary(storm)
view(storm)

length(unique(storm$EVTYPE))

events <- tolower(storm$EVTYPE)
events <- gsub("[[:blank:][:punct:]+]", " ", events)
length(unique(events))

storm$EVTYPE <- events

#Health and events
health <- ddply(storm, .(EVTYPE), summarize, Fatalities = sum(FATALITIES), Injuries = sum(INJURIES))
top.fatalities <- data.table(head(health[order(health$Fatalities, decreasing = T), 1:2], 20))

heat <- c(which(top.fatalities$EVTYPE == "heat"),
          which(top.fatalities$EVTYPE == "excessive heat"),
          which(top.fatalities$EVTYPE == "heat wave"))

top.fatalities$Fatalities[heat[1]] <- sum(top.fatalities$Fatalities[heat])
top.fatalities <- top.fatalities[-heat[2:3],]

flood <- c(which(top.fatalities$EVTYPE == "flood"),
           which(top.fatalities$EVTYPE == "flash flood"))

top.fatalities$Fatalities[flood[1]] <- sum(top.fatalities$Fatalities[flood])
top.fatalities <- top.fatalities[-flood[2],]

thunderstorm <- c(which(top.fatalities$EVTYPE == "thunderstorm wind"),
                  which(top.fatalities$EVTYPE == "tstm wind"))

top.fatalities$Fatalities[thunderstorm[1]] <- sum(top.fatalities$Fatalities[thunderstorm])
top.fatalities <- top.fatalities[-thunderstorm[2],]

wind <- c(which(top.fatalities$EVTYPE == "strong wind"),
          which(top.fatalities$EVTYPE == "high wind"))

top.fatalities$Fatalities[wind[1]] <- sum(top.fatalities$Fatalities[wind])
top.fatalities <- top.fatalities[-wind[2],]

winter.storm <- c(which(top.fatalities$EVTYPE == "winter storm"),
                  which(top.fatalities$EVTYPE == "blizzard"))

top.fatalities$Fatalities[winter.storm[1]] <- sum(top.fatalities$Fatalities[winter.storm])
top.fatalities <- top.fatalities[-winter.storm[2],]

cold <- c(which(top.fatalities$EVTYPE == "extreme cold"),
          which(top.fatalities$EVTYPE == "extreme cold wind chill"))

top.fatalities$Fatalities[cold[1]] <- sum(top.fatalities$Fatalities[cold])
top.fatalities <- top.fatalities[-cold[2],]


top.fatalities <- head(top.fatalities[order(top.fatalities$Fatalities, decreasing = T),],10)

####

top.injuries <- data.table(tail(health[order(health$Injuries), c(1,3)], 20))

heat <- c(which(top.injuries$EVTYPE == "heat"),
          which(top.injuries$EVTYPE == "excessive heat"))

top.injuries$Injuries[heat[1]] <- sum(top.injuries$Injuries[heat])
top.injuries <- top.injuries[-heat[2],]

flood <- c(which(top.injuries$EVTYPE == "flood"),
           which(top.injuries$EVTYPE == "flash flood"))

top.injuries$Injuries[flood[1]] <- sum(top.injuries$Injuries[flood])
top.injuries <- top.injuries[-flood[2],]

thunderstorm <- c(which(top.injuries$EVTYPE == "thunderstorm wind"),
                  which(top.injuries$EVTYPE == "thunderstorm winds"),
                  which(top.injuries$EVTYPE == "tstm wind"))

top.injuries$Injuries[thunderstorm[1]] <- sum(top.injuries$Injuries[thunderstorm])
top.injuries <- top.injuries[-thunderstorm[2:3],]

winter.storm <- c(which(top.injuries$EVTYPE == "winter storm"),
                  which(top.injuries$EVTYPE == "blizzard"))

top.injuries$Injuries[winter.storm[1]] <- sum(top.injuries$Injuries[winter.storm])
top.injuries <- top.injuries[-winter.storm[2],]


wildfire <- c(which(top.injuries$EVTYPE == "wildfire"),
              which(top.injuries$EVTYPE == "wild forest fire"))

top.injuries$Injuries[wildfire[1]] <- sum(top.injuries$Injuries[wildfire])
top.injuries <- top.injuries[-wildfire[2],]

top.injuries <- head(top.injuries[order(top.injuries$Injuries, decreasing = T),],10)

library(ggplot2)
library(gridExtra)

graph1 <- ggplot(data = top.fatalities, aes(x = reorder(EVTYPE, Fatalities), y = Fatalities, fill = Fatalities)) +
          geom_bar(stat="identity") +
          ylab("Total Fatalities") +
          xlab("") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
####
graph2 <- ggplot(data = top.injuries, aes(x = reorder(EVTYPE, Injuries), y = Injuries, fill = Injuries)) +
          geom_bar(stat="identity") +
          ylab("Total Injuries") +
          xlab("") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
grid.arrange(graph1, graph2, ncol = 2, main = "Most Harmful Weather Events in the US from 1950 to 2011")


##### Economic Cost

storm$PROPDMGEXP <- tolower(storm$PROPDMGEXP)
storm$CROPDMGEXP <- tolower(storm$CROPDMGEXP)

multiplier <- function(x){
    if (x == 'h')
        return(100)
    else if (x == 'k')
        return(10**3)
    else if (x == 'm')
        return(10**6)
    else if (x == 'b')
        return(10**9)
    else if (!is.na(as.numeric(x)))
        return(10**as.numeric(x))
    else if (x %in% c('', '-', '?', '+'))
        return(1)
    else {
        stop("Invalid exponent value.")
    }
}

propMulti <- sapply(storm$PROPDMGEXP, FUN = multiplier) 
cropMulti <- sapply(storm$CROPDMGEXP, FUN = multiplier) 

storm$PropCost <- storm$PROPDMG * propMulti
storm$CropCost <- storm$CROPDMG * cropMulti
storm$TotalCost <- storm$PropCost + storm$CropCost

cost <- ddply(storm, .(EVTYPE), summarize,
              TotalCost = sum(TotalCost))
top.costs <- data.table(head(cost[order(cost$TotalCost, decreasing = T), ], 20))

flood <- c(which(top.costs$EVTYPE == "flood"),
           which(top.costs$EVTYPE == "flash flood"),
           which(top.costs$EVTYPE == "river flood"))

top.costs$TotalCost[flood[1]] <- sum(top.costs$TotalCost[flood])
top.costs <- top.costs[-flood[2:3],]

storm <- c(which(top.costs$EVTYPE == "winter storm"),
           which(top.costs$EVTYPE == "ice storm"))

top.costs$TotalCost[storm[1]] <- sum(top.costs$TotalCost[storm])
top.costs <- top.costs[-storm[2],]

thunderstorm <- c(which(top.costs$EVTYPE == "thunderstorm wind"),
                  which(top.costs$EVTYPE == "tstm wind"))

top.costs$TotalCost[thunderstorm[1]] <- sum(top.costs$TotalCost[thunderstorm])
top.costs <- top.costs[-thunderstorm[2],]

top.costs <- head(top.costs[order(top.costs$TotalCost, decreasing = T),],10)

graph3 <- ggplot(data = top.costs, aes(x = reorder(EVTYPE, TotalCost), y = TotalCost, fill = TotalCost)) +
    geom_bar(stat="identity") +
    ylab("Total Cost (USD)") +
    xlab("") +
    ggtitle("Property and Crop Damage Costs\nfrom Weather Events in the US from 1950 to 2011") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
graph3


fatalities.costs <- merge(top.fatalities, top.costs, by = "EVTYPE")
graph4 <- ggplot(data = fatalities.costs, aes(x = Fatalities, y = TotalCost, label = EVTYPE)) +
          geom_point() +
          ylab("Total Cost (USD)") +
          ylim(0, 1.9*(10**11)) +
          xlab("Total Fatalities ") +
          ggtitle("Top 4 Most Costly and Deadly Weather Event\nin the US from 1950 to 2011") +
          geom_text(angle=45, vjust=-1, hjust=0, size=3)
graph4

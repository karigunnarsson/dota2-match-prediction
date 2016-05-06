library(dplyr)
library(ggplot2)
library(RColorBrewer)

## Summarise the average number of kills per minute. We remove the NA so that we are not counting
## games where the game ended quickly as zero (if a hero for example usually destroys games and finishes
## the game early, it's wrong to set all minutes after to zero)
averageKills <- dplyr::group_by(outputFile[,c(4,8:24)], heroID) %>%
    dplyr::summarise_each(funs(mean(., na.rm = TRUE)))

averageKills <- averageKills[,c(1:8)]

averageKillsLong <- tidyr::gather(averageKills, "heroID")
colnames(averageKillsLong)[2] <- "time" 
averageKillsLong$time <- as.numeric(averageKillsLong$time)

# Read in hero names
heroNames <- read.csv2("~/yasp/heronames.csv")
heroInfo <- read.csv("~/yasp/heroInfo.csv", sep=";",
                     colClasses = c("factor","factor","factor","numeric","numeric","numeric","numeric", "numeric"))

withHeroNames <- merge(averageKillsLong, heroInfo, by.x = "heroID", by.y = "hero_id", all.x = TRUE)

# For some reason the Meepo power curve is absolutely insne, remove it for now since it messes up the scale
heroListClean <- subset(withHeroNames, !is.na(withHeroNames$hero_name) & withHeroNames$hero_name != "Meepo")

heroStr <- subset(heroListClean, heroListClean$mainStat == "strength") 
heroAgi <- subset(heroListClean, heroListClean$mainStat == "agility") 
heroInt <- subset(heroListClean, heroListClean$mainStat == "intelligence") 

ggplot(heroListClean, aes(x=time, y=value, fill = mainStat)) + geom_area() + 
    scale_fill_brewer(palette="Set1") + facet_wrap(~ hero_name) 

## Now to calculate average tower damage, stuns and heal per hero.

averageOther <- dplyr::group_by(outputFile[,c(4:7)], heroID) %>%
    dplyr::summarise_each(funs(mean(., na.rm = TRUE)))

# Add to the hero info list
heroInfoTotal <- merge(heroInfo, averageOther, by.x = "hero_id", by.y = "heroID", all.x = TRUE)

# Add the power curve to the same list
heroInfoTotal <- merge(heroInfoTotal, averageKills, by.x = "hero_id", by.y = "heroID", all.x = TRUE)

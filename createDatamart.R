#######
# Wrangle all the data into one big string per match for machine learning.
#######
library(tidyr)

# First off, i saw in previous step that there is a "heroID = 0" which does not exist, we remove all matches with that ID

zeroIDMatchList <- unique(subset(outputFile, outputFile$heroID == 0)$matchID)
gameListClean <- outputFile$matchID %in% zeroIDMatchList

gamesClean <- outputFile[!gameListClean,]
gamesClean <- gamesClean[,c(1:4)]

# User player slot to find out if it's Radiant or Dire.
gamesClean$side <- ifelse(gamesClean$playerSlot < 10, "Radiant", "Dire")

# Add all the hero info on as well
withHeroInfo <- merge(gamesClean, heroInfoTotal[,c(1,4:18)], by.x = "heroID", by.y = "hero_id", all.x = TRUE)

# Sum up all the hero info per side per match

averagePerSide <- dplyr::group_by(withHeroInfo[,c(2,3,5:20)], matchID, side, radiantWin) %>%
    dplyr::summarise_each(funs(sum(., na.rm = TRUE)))

# Average radiant, and rename
averageRadiant <- subset(averagePerSide, averagePerSide$side == "Radiant")
colnames(averageRadiant) <- c("matchID","side","radiantWin","rLockdown","rMagicBurst3s","rHeal3s",
                              "rAOEBurst","rTeamfight","rTD","rHH","rStuns","r0","r10","r20","r30",
                              "r40","r50","r60")

# Average dire, and rename
averageDire <- subset(averagePerSide, averagePerSide$side == "Dire")
colnames(averageDire) <- c("matchID","side","radiantWin","dLockdown","dMagicBurst3s","dHeal3s",
                              "dAOEBurst","dTeamfight","dTD","dHH","dStuns","d0","d10","d20","d30",
                              "d40","d50","d60")

# Combine the two
averageTotal <- merge(averageDire, averageRadiant, by=c("matchID","radiantWin"), all = TRUE)

# Create variables for all power curves so they are relative, we want to know how strong one team is relative
# to the other, absolute numbers alone are not very exciting.
averageTotal$t0 <- averageTotal$r0 / averageTotal$d0
averageTotal$t10 <- averageTotal$r10 / averageTotal$d10
averageTotal$t20 <- averageTotal$r20 / averageTotal$d20
averageTotal$t30 <- averageTotal$r30 / averageTotal$d30
averageTotal$t40 <- averageTotal$r40 / averageTotal$d40
averageTotal$t50 <- averageTotal$r50 / averageTotal$d50
averageTotal$t60 <- averageTotal$r60 / averageTotal$d60

# Now drop the side specific power curve variables, and we are finished.
averageTotal$result <- as.factor(ifelse(averageTotal$radiantWin == TRUE, "Radiant Win", "Dire Win"))
finalFile <- averageTotal[,c(4:11,20:27,35:42)]

saveRDS(finalFile, "learningDatamart.RDS")

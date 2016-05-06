# Load the libraries
library(jsonlite)
library(data.table)
library(plyr)

#######
# Unfortunately none of the existing streaming packages seemed to work for the format that YASP has used (with new lines
# for commas and the opening/end brakcets), so i have to build my own little function to do it for me.
# Not sure if it's as efficient, but it takes ~0,4s per match on my computer (stable), and stays at a steady 160mb RAM used.
# So at least it scales well, though it may take more time to parse than jsonlites stream_in function
#######

# Initiate a file connection
fileCon <- gzfile('C:/Users/dracovich/Documents/yasp/yasp-dump-2015-12-18.json.gz', open="rb")

# Initiate all variables needed for the homemade stream function

numMatches <- 50000
outputFile <- 0
lineCount <- 0
matchCount <- 0

# Stream in using readLines until we reach the number of matches we want.
while(matchCount < numMatches) {
    next_line = readLines(fileCon, n = 1)
    
    
    lineCount <- lineCount + 1

    # Only look at even numbered lines to avoid the [ and , lines.    
    if(lineCount %% 2 == 0) {
        
        print(matchCount)
        # read into JSON format
        readJSON <- jsonlite::fromJSON(next_line)
        
        # Up the match counter
        matchCount <- matchCount + 1
        
        gameTime <- round((readJSON$duration/60) / 10, digits = 0) * 10
        # Cycle through each hero in the match, creating a list of kills, and other info.
        for(j in 1:10) {
            killList <- rbindlist(readJSON$players$kills_log[j], fill=TRUE)
            
            # Some heroes have zero kills all game, those have to be initilaized, rest gets calculated.
            if (length(killList) == 0 ){
                killSummed <- data.frame(time = 0, n = 0)
            } else {
                killList <- as.data.frame((round((killList$time / 60) / 10, digits = 0)) * 10)
                colnames(killList)[1] <- "time"
                killSummed <- as.data.frame(dplyr::count(killList, time))
            }
            
            # need to do a little song and dance to make sure that all values between the total gametime and zero
            # are filled out (since NA values will be treated in a special way so we get good values for
            # games that are short).
            
            timeList <- killSummed$time 
            for (k in seq(from = 0, to = gameTime, by = 10)) { 
                if (k %in% timeList) {
                    next
                } else {emptyTime <- data.frame(time = k, n = 0)
                killSummed <- rbind(killSummed, emptyTime)
                }
            }
            
            killSummed <- killSummed[order(killSummed$time),]
            
            # Create a herostring, adding tower damage and some other info for later use. Also add matchID so we can
            # use this as a basis for the dataset with heroes per match
            heroString <- cbind(readJSON$match_id,
                                readJSON$radiant_win,
                                readJSON$players$player_slot[j],
                                readJSON$players$hero_id[j],
                                readJSON$players$tower_damage[j],
                                readJSON$players$hero_healing[j],
                                readJSON$players$stuns[j],
                                tidyr::spread(killSummed, time, n))
            colnames(heroString)[1:7] <- c("matchID","radiantWin","playerSlot","heroID","towerDamage","heroHealing","stuns")
            
            heroString[is.na(heroString)] <- 0
            if (j == 1) {
                matchList <- heroString
            } else {
                matchList <- rbind.fill(matchList, heroString)
            }
        }
        
        if (length(outputFile) == 1) {
            outputFile <- matchList
        } else {
            outputFile <- rbind.fill(outputFile, matchList)
        } 
    }
}

saveRDS(outputFile, "yaspMatches.RDS")

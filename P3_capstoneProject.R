# import necessary libraries 
library('tidyverse')

# import the files as tibbles
batting <- read_csv('./dataFiles/Batting.csv', name_repair = make.names)
sal <- read_csv('./dataFiles/Salaries.csv', name_repair = make.names)

# calculate some key stats from batting
# - H: total Hits
# - AB: At Bat
# - BB: Bases on Balls (Walks)
# - HBP: Hit By Pitch
# - SF: Sacrifice Fly
# - NB: number times made N hits
# - HR: Home Runs
batting <- batting %>% mutate(BA = H / AB) # Batting Average 
batting <- batting %>% mutate(
  OBP = (H + BB + HBP) / (AB + BB + HBP + SF)) # On Base Perc.
batting <- batting %>% mutate(
  X1B = (H - X2B - X3B - HR), .before=X2B) # num. single hits
batting <- batting %>% mutate(
  SLG = ((X1B) + (2 * X2B) + (3 * X3B) + (4 * HR)) / AB) # Slugging Perc.

# remove year IDs that don't match salary data
batting <- batting %>% subset(yearID >= min(sal$yearID))

# merge batting data and salary data
combo <- merge(batting,sal,by = c('playerID','yearID'))

# get data for lost players
lost_players <- combo %>% subset(
  playerID %in% c('giambja01', 'damonjo01', 'saenzol01') & yearID == 2001,
  select = c('playerID', 'H', 'X2B', 'X3B', 'HR', 'OBP', 'SLG', 'BA', 'AB'))

# get statistics we want to match on
targSal <- 15e6 # target salary - 15 million
targAB <- sum(lost_players$AB) # target AB >= sum lost players AB
targOBP <- mean(lost_players$OBP) # target OBP >= mean lost players OBP 

# ------------------------------------------------------------------------------  
# time to find some players! (want 3)

# get a data frame for replacement options
new_players <- combo %>% subset(
  !(playerID %in% c('giambja01', 'damonjo01', 'saenzol01')) & yearID == 2001)

# Use targets to create thresholds for data frame
# - salary cannot exceed 15 mill
# - min AB must be enough to add to max two ABs for target
# - min OBP must be enough to add to max two OBPs for target
maxSal <- targSal - sum(sort(new_players$salary)[1:2])
minAB <- targAB - sum(sort(new_players$AB, decreasing = TRUE)[1:2])
minOBP <- (3 * targOBP) - sum(sort(new_players$OBP, decreasing = TRUE)[1:2])

# use above to restrict available players
new_players <- new_players %>% subset(
  salary <= maxSal & AB >= minAB & OBP >= minOBP,
  select = c('playerID','salary','AB','OBP'))

# lets see how many players would contribute > 1/3 of AB, OBP targets
great_players <- new_players %>% subset(
  AB > targAB/3 & OBP > targOBP/3)
nGreat <- nrow(great_players)

# calculate number of combinations
nCom = choose(nGreat,3) 
# 192k - manageable to do full search!

# work out which exclusion criterion is most likely to hit
exclChance <- great_players[c('salary', 'AB', 'OBP')] %>% mutate(
  across(c('salary', 'AB', 'OBP'), ~ .x + 2*mean(.x))) %>% mutate(
    salary = salary > targSal, AB = AB < targAB, OBP = OBP < 3*targOBP)
print(colSums(exclChance))
# OBP (72) > salary (28) > AB (0)... rearrange comparisons accordingly

# create matrix version of great_players for faster indexing
mat_gPlayers <- data.matrix(great_players[c('salary','AB','OBP')])

# loop timing optimisation:
# - basic run: 8.26s
# - rearranging comparison operators (currVals): 7.91s
# - using matrix for indexing over data frame: 0.74s!
# - short-circuit comparisons (&& not &): 0.45s
# - only update progress bar every 10 items: 0.41s
# - transposing matrix made no difference (although R col. major)
# - progress bar was inside wrong loop (p2), now update every item, 0.45s
# - one last thing, addition (p1vals+p2vals) now incremental: 0.36s

# loop over combos
stTime <- Sys.time()
idx <- 1
valCombos <- vector(mode='list', length=nCom)
pb <- txtProgressBar(min = 0, max = (nGreat-2))
for (p1 in 1:(nGreat-2)){
  
  p1Vals <- mat_gPlayers[p1,]
  
  for (p2 in (p1+1):(nGreat-1)) {
    
    p2Vals <- p1Vals + mat_gPlayers[p2,]
    
    for (p3 in (p2+1):nGreat) {

      p3Vals <- p2Vals + mat_gPlayers[p3,]
      
      # comparisons ordered from least to most likely
      if (p3Vals[3] >= 3*targOBP &&
          p3Vals[1] <= targSal &&
          p3Vals[2] >= targAB) {
        valCombos[[idx]] <- c(p1,p2,p3,p3Vals)
        idx <- idx + 1
      }
    }
  }
  setTxtProgressBar(pb,p1)
}
close(pb)
endTime <-Sys.time()
sprintf('Took %.2fs',endTime-stTime)

# clean up valCombos and convert to data frame
# (using sapply to apply transpose)
valCombos <- tibble(data.frame(t(sapply(valCombos[1:idx-1],c))))
colnames(valCombos) <- c('P1','P2','P3','totSal','totAB','avgOBP')

# insert player id to dataframe
valCombos <- valCombos %>% mutate(
  P1_ID = great_players$playerID[valCombos$P1], 
  P2_ID = great_players$playerID[valCombos$P2], 
  P3_ID = great_players$playerID[valCombos$P3], 
  .before = totSal
  )

# convert OBP from sum to mean
valCombos <- valCombos %>% mutate(avgOBP = avgOBP/3)

# calculate z-scores
# (reversing salary as lower = better!)
valCombos <- valCombos %>% mutate(
  Sal_Z = -1 * ((totSal-mean(totSal))/sd(totSal)),
  AB_Z = (totAB-mean(totAB))/sd(totAB),
  OBP_Z = (avgOBP-mean(avgOBP))/sd(avgOBP)
  )

# calculate sum z-scores
valCombos <- valCombos %>% mutate(sum_Z = Sal_Z + AB_Z + OBP_Z)

# sort data frame by best z score
valCombos <- arrange(valCombos,desc(sum_Z))

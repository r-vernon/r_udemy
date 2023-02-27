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

# loop over combos
idx <- 1
valCombos <- vector(mode='list', length=nCom)
pb <- txtProgressBar(min = 0, max = (nGreat-2))
for (p1 in 1:(nGreat-2)){
  
  p1Vals <- as.numeric(great_players[p1,c('salary','AB','OBP')])
  
  for (p2 in (p1+1):(nGreat-1)) {
    
    p2Vals <- as.numeric(great_players[p2,c('salary','AB','OBP')])
    
    for (p3 in (p2+1):nGreat) {
      
      p3Vals <- as.numeric(great_players[p3,c('salary','AB','OBP')])
      currVals <- p1Vals + p2Vals + p3Vals 
      
      if (currVals[1] <= targSal & 
          currVals[2] >= targAB & 
          currVals[3] >= 3*targOBP) {
        valCombos[[idx]] <- c(p1,p2,p3,currVals)
        idx <- idx + 1
      }
    }
    setTxtProgressBar(pb,p1)
  }
}
close(pb)

# clean up valCombos and convert to data frame
# (using sapply to apply transpose)
test <- tibble(data.frame(t(sapply(valCombos[1:idx-1],c))))
colnames(test) <- c('P1','P2','P3','Sal','AB','OBP')

# calculate sum z-scores
test <- test %>% mutate(OBP = OBP/3)

# calculate z-scores
test <- test %>% mutate(
  Sal_Z = -1 * ((Sal-mean(Sal))/sd(Sal)),
  AB_Z = (AB-mean(AB))/sd(AB),
  OBP_Z = (OBP-mean(OBP))/sd(OBP)
  )

# calculate sum z-scores
test <- test %>% mutate(sum_Z = Sal_Z + AB_Z + OBP_Z)

# sort data frame by best z score
test <- arrange(test,desc(sum_Z))

library(tidyverse)
library(readr)
library(dplyr)

saveRDS(KpiTester, file = "tm2024clean.RDS")

#Load in game data, can be any file
kpiTester <- readRDS("data/tm2024clean.RDS")

KpiTester <- tm2024test

# THIS AND THE FOR LOOP IS THE MAIN THING
totalKPI <- NA

Dates <- na.omit(unique(KpiTester$Date))

for (date in Dates) {
  kpiDate <- KpiTester %>% filter(Date == date)
  
  # Batters Faced & Three Pitch Outs
  #Creating the table with Batters Faced by each pitcher and Number of three pitch outs
  pitcher_stats <- kpiDate %>%
    group_by(Pitcher) %>%
    summarize(
      # Calculate number of batters faced by each pitcher
      BattersFaced = sum(HitType != "Undefined" | KorBB == "Strikeout" | KorBB == "Walk"|PitchCall == "HitByPitch"), 
      #batters_faced = sum(PitchofPA = 1),
      #Calculate number of 3 pitch outs
      ThreePO = sum((PlayResult == "Out" | KorBB == "Strikeout"| PlayResult == "Sacrifice") & PitchofPA <= 3),
      TotalRuns = sum(RunsScored),
    )
  
  pitcher_stats$Date <- date
  
  # Ahead in Three
  #Calculate the number of ahead in threes
  Ahead3 <- kpiDate %>%
    filter((Balls == 0 & Strikes == 2) | (Balls == 1 & Strikes == 2))
  Ahead3 <- Ahead3 %>%
    group_by(Pitcher) %>%
    summarize(Ahead3 = n_distinct(paste(Inning ,PAofInning)))
  #Join the ahead in three data with the pitcher_stats table
  pitcher_stats <- left_join(pitcher_stats, Ahead3, by = c("Pitcher" = "Pitcher"))
  
  # Walks & Strikeouts
  trueOutcomeData <- kpiDate %>%
    group_by(Pitcher) %>%
    summarize(
      BB = sum(KorBB == "Walk" | PitchCall == "HitByPitch"),
      Strikeouts = sum(KorBB == "Strikeout")
    )
  pitcher_stats <- left_join(pitcher_stats, trueOutcomeData, by = c("Pitcher" = "Pitcher"))
  
  # Shutdown Inning Stuff
  #Total number of shutdown inning calculation
  #KpiShutdown <- kpiDate %>%
    #Grouping by half inning
    #group_by(Inning, Top.Bottom) %>%
    #Make a run scored column which has 1 if a run was scored 0 if not
    #mutate(InningScore = ifelse(sum(RunsScored) > 0, 1, 0)) 
  KpiShutdown <- NA
  
  for(i in unique(kpiDate$SessionID)){
    test <- kpiDate[kpiDate$SessionID == i,]
    testGrouping <- test %>%
      group_by(Inning, Top.Bottom) %>%
      #Make a run scored column which has 1 if a run was scored 0 if not
      mutate(InningScore = ifelse(sum(RunsScored) > 0, 1, 0)) 
    if (!is.null(KpiShutdown) && is.data.frame(KpiShutdown) && nrow(KpiShutdown) > 0) {
      KpiShutdown <- rbind(KpiShutdown,testGrouping)
    } else {
      KpiShutdown <- testGrouping
    }
  }

  KpiShutdown <- KpiShutdown %>%
    group_by(Pitcher, Inning) %>%
    mutate(TwoOuts = ifelse(PitcherTeam == "WAK_DEA" & sum(OutsOnPlay | KorBB == "Strikeout") >= 2, 1,0)) %>%
    mutate(AnyTwoOuts = ifelse(sum(OutsOnPlay | KorBB == "Strikeout") >= 2, 1, 0))
  
  if (sum(KpiShutdown$AnyTwoOuts) > 0) {
    KpiShutdown <- KpiShutdown %>%
      filter(AnyTwoOuts == 1) %>%
      filter(row_number(PitchNo) == 1)
    
  } else {
    KpiShutdown <- KpiShutdown %>%
      filter(row_number(PitchNo) == 1)
  }
  
  KpiShutdown$previous_score <- NA
  for (i in 2:nrow(KpiShutdown)) {
    # check if current row's pitcher is on the WAK_DEA team
    #if (KpiShutdown$PitcherTeam[i] == "WAK_DEA") {
      # if yes, get the previous inning score
      prev_score <- KpiShutdown$InningScore[i-1]
      # store the previous inning score in the new column
      KpiShutdown$previous_score[i] <- prev_score
    #}
  }
  
  
  KpiShutdown$previous_score[is.na(KpiShutdown$previous_score)] <- 0
  
  # create a new column to indicate if there was a shutdown opportunity
  KpiShutdown$shutdownChance <- ifelse(KpiShutdown$previous_score >= 1, 1, 0)
  KpiShutdown <- select(KpiShutdown, -previous_score)
  #Create a new column to indicate if there was a shutdown success
  KpiShutdown <- KpiShutdown %>%
    mutate(shutdown_success = ifelse(InningScore == 0 & shutdownChance == 1, 1, 0))
  #KpiShutdown = KpiShutdown %>% filter(PitcherTeam == "WAK_DEA")
  
  KpiShutdownNumber <- KpiShutdown %>%
    group_by(Pitcher) %>%
    summarize(
      SdInning = sum(shutdown_success),
      SdInningOpp = sum(shutdownChance)
    )
  #view(KpiShutdownNumber)
  pitcher_stats <- left_join(pitcher_stats, KpiShutdownNumber, by = c("Pitcher" = "Pitcher"))
  
  # Innings Pitched
  
  KpiInnings <- kpiDate %>%
    group_by(Pitcher) %>%
    mutate(equal_to_next_outs = case_when(
      Outs == lead(Outs) ~ 1L,
      TRUE ~ 0L
    )) %>%
    filter(!is.na(OutsOnPlay)) %>%
    summarize(
      totalOuts = sum(OutsOnPlay),
      totalK = sum(KorBB == "Strikeout" & equal_to_next_outs == 0),
      TotalPitcherOut = totalOuts + totalK
    )
  
  KpiInnings <- select(KpiInnings, -totalOuts, -totalK)
  KpiInnings$IP <- floor(KpiInnings$TotalPitcherOut / 3) + (KpiInnings$TotalPitcherOut %% 3) / 10
  pitcher_stats <- left_join(pitcher_stats, KpiInnings, by = c("Pitcher" = "Pitcher"))
  
  #Which innings they pitched to determine order
  KpiPitchNo <- kpiDate %>%
    group_by(PitcherId) %>%
    mutate(FirstPitchNo = ifelse(row_number(PitchNo) == min(row_number(PitchNo)), PitchNo, NA)) %>%
    filter(!is.na(FirstPitchNo))
  KpiPitchNo <- select(KpiPitchNo, Pitcher, PitchNo)
  pitcher_stats <- left_join(pitcher_stats, KpiPitchNo, by = c("Pitcher" = "Pitcher"))
  
  # Freebies
  kpiDate$freebie <- 0
  kpiDate$freebie[which(kpiDate$KorBB == "Walk" | kpiDate$PitchCall == "HitByPitch" | kpiDate$PlayResult == "Error")] <- 1
  kpiFreebie <- kpiDate %>%
    group_by(Pitcher) %>%
    summarize(
      Freebie = sum(freebie),
    )
  pitcher_stats <- left_join(pitcher_stats, kpiFreebie, by = c("Pitcher" = "Pitcher"))
  
  oafOpp <- which(kpiDate$KorBB == "Walk" | kpiDate$PitchCall == "HitByPitch" | kpiDate$PlayResult == "Error") + 1
  finalABlength <- 1
  j <- nrow(kpiDate)
  while(kpiDate$PAofInning[j] == kpiDate$PAofInning[j-1]){
    finalABlength <- finalABlength + 1
    j <- j-1
  }
  
  kpiDate$oaf <- 0
  for(i in oafOpp){
    oafRecorded <- FALSE
    if(i < (nrow(kpiDate) - finalABlength + 1)) {
      k <- i
      while(kpiDate$PAofInning[k] == kpiDate$PAofInning[k+1] & !is.na(kpiDate$PAofInning[k+1])){
        if(!oafRecorded){
          if(kpiDate$OutsOnPlay[k] > 0 | kpiDate$KorBB[k] == "Strikeout"){
            kpiDate$oaf[k] <- 1
            oafRecorded <- TRUE
          }
        }
        k <- k+1
      }
      if(!oafRecorded){
        if((kpiDate$OutsOnPlay[k] > 0 | kpiDate$KorBB[k] == "Strikeout") & !is.na(kpiDate$PAofInning[k+1])){
          kpiDate$oaf[k] <- 1
          oafRecorded <- TRUE
        }
      }
    } else {
      if(i <= nrow(kpiDate)  & !is.na(kpiDate$PAofInning[k+1])){
        for(k in i:nrow(kpiDate)) {
          if(!oafRecorded & !is.na(kpiDate$PAofInning[k+1])){
            if(kpiDate$OutsOnPlay[k] > 0 | kpiDate$KorBB[k] == "Strikeout"){
              kpiDate$oaf[k] <- 1
              oafRecorded <- TRUE
            }
          }
        }
      }
    }
  }
  
  kpiOaf <- kpiDate %>%
    group_by(Pitcher) %>%
    summarize(
      OutsAfterFreebie = sum(oaf),
    )
  pitcher_stats <- left_join(pitcher_stats, kpiOaf, by = c("Pitcher" = "Pitcher"))
  
  # Stolen Bases
  kpiSB <- kpiDate %>%
    group_by(Pitcher) %>%
    summarize(
      StolenBases = sum(stolenBase2B == "true") + sum(stolenBase3B == "true") + sum (stolenBaseHome == "true"), 
      StolenBaseAttempts = sum(stolenBaseAttempt2B == "true") + sum(stolenBaseAttempt3B == "true") + sum(stolenBaseAttemptHome == "true"), 
    )
  pitcher_stats <- left_join(pitcher_stats, kpiSB, by = c("Pitcher" = "Pitcher"))
  
  # Set any NA stats equal to 0
  pitcher_stats[is.na(pitcher_stats)] <- 0
  
  # Remove PitchNo column
  pitcher_stats <- pitcher_stats %>%
    select(-PitchNo)
  
  # Combine at the end
  if (!is.null(totalKPI) && is.data.frame(totalKPI) && nrow(totalKPI) > 0) {
    totalKPI <- rbind(totalKPI,pitcher_stats)
  } else {
    totalKPI <- pitcher_stats
  }
}

#-------------------------
#Joining the pitcher_stats table with the Kpi CSV file
#KPI_Tracker_App <- read_csv("Downloads/App Stuff/PracticeApp/data/KPI_Tracker.csv")
KPI_Tracker_App <- bind_rows(KPI_Tracker_App, pitcher_stats)
#write_csv(KPI_Tracker_App, "Downloads/KPI_Tracker.csv")

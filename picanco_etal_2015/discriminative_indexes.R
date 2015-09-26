source("load_frame_objects.R")

discriminativeIndex <- function(frameObject) {
# We must walk through the frame objects with an increment of 5
# to calculate all the discriminative indexes of the corresponding stage (A^1, B^2, A^2 or B^2):
  incrementDI <- 5
  lengthDI <- (nrow(frameObject) / incrementDI)
  DI <- numeric(lengthDI)
  from <- 1
  to <- incrementDI

  for (i in 1:lengthDI){
    ratePositive <- sum(frameObject$cfposi[from:to])/sum(frameObject$ctposi[from:to]) 
    rateNegative <- sum(frameObject$cfnega[from:to])/sum(frameObject$ctnega[from:to]) 

    DI[i] <- ratePositive / (ratePositive + rateNegative)
    from <- from + incrementDI
    to <- to + incrementDI
  }

  frameObjectName <- deparse(substitute(frameObject))
  print(paste("DI (Discriminative Indexes) were created from:",frameObjectName))
  DI
}

discriminativeIndexesMean <- function(DI) { # Just for graphication purposes...
  # stage_list$DI. 1-4 (5-8 and so on)
  # gives you the indexes of the 4 relations of the corresponding session.
  # We must walk through the frameObject.DI with an increment of 4
  # to calculate the statistics of the discriminative indexes session by session:  
  incrementMDI <- 4
  lengthMDI <- length(DI)/incrementMDI
  MDI <- numeric(lengthMDI)
  from <- 1
  to <- incrementMDI

  for (i in 1:lengthMDI){
    MDI[i] <- mean(DI[from:to]) 
    from <- from + incrementMDI
    to <- to + incrementMDI
  }
  frameObjectName <- deparse(substitute(DI))
  print(paste("MDI (Mean Discriminative Index) were created from:",frameObjectName))
  MDI
}

#Bongo
Bongo$stageA1$DI = discriminativeIndex(Bongo$stageA1$data)
Bongo$stageA1$MDI = discriminativeIndexesMean(Bongo$stageA1$DI)

Bongo$stageB1$DI = discriminativeIndex(Bongo$stageB1$data)
Bongo$stageB1$MDI = discriminativeIndexesMean(Bongo$stageB1$DI)

# Negão
Negao$stageA1$DI = discriminativeIndex(Negao$stageA1$data)
Negao$stageA1$MDI = discriminativeIndexesMean(Negao$stageA1$DI)

Negao$stageB1$DI = discriminativeIndex(Negao$stageB1$data)
Negao$stageB1$MDI = discriminativeIndexesMean(Negao$stageB1$DI)

Negao$stageA2$DI = discriminativeIndex(Negao$stageA2$data)
Negao$stageA2$MDI = discriminativeIndexesMean(Negao$stageA2$DI)

Negao$stageB2$DI = discriminativeIndex(Negao$stageB2$data)
Negao$stageB2$MDI = discriminativeIndexesMean(Negao$stageB2$DI)
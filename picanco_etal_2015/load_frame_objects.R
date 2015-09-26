minstr <- function(objectData){
  # we call $data as frameObject
  # - Each row is associated with a positive AND a negative trial.
  # - rows from 1 to 20 gives you the first session, 21-40 the second, and so on.
  # - 1-5 (6-10, and so on) gives you all the sample-comparison pairs (e.g., A1-B1 and A1-B2),
  #   the so called discriminative relation, presented during the corresponding session.
  print(deparse(substitute(objectData)))
  for(i in ls(objectData)){
    print(paste0(i,'$data'))
 }  
}

currentDirectory <- getwd()
writeLines("")
writeLines(paste("Current working directory:", currentDirectory))
# file.path(..., fsep = .Platform$file.sep)

# Bongo
stageA1 <- list(data=read.table(file.path("METADATA","BONGO_01_SIMETRICO.txt"), sep="\t", header=TRUE))
stageB1 <- list(data=read.table(file.path("METADATA","BONGO_02_ASSIMETRICO.txt"), sep="\t", header=TRUE))
Bongo <- list(stageA1=stageA1, stageB1=stageB1)

# Negão
stageA1 <- list(data=read.table(file.path("METADATA","NEGAO_01_SIMETRICO.txt"), sep="\t", header=TRUE))
stageB1 <- list(data=read.table(file.path("METADATA","NEGAO_02_ASSIMETRICO.txt"), sep="\t", header=TRUE))
stageA2 <- list(data=read.table(file.path("METADATA","NEGAO_03_SIMETRICO.txt"), sep="\t", header=TRUE))
stageB2 <- list(data=read.table(file.path("METADATA","NEGAO_04_ASSIMETRICO.txt"), sep="\t", header=TRUE))
Negao <- list(stageA1=stageA1, stageB1=stageB1, stageA2=stageA2, stageB2=stageB2)

remove(currentDirectory, stageA1, stageB1, stageA2, stageB2)
writeLines("The following lists were built:")

minstr(Bongo)
minstr(Negao)
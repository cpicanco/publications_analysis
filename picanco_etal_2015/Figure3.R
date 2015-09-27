source("discriminative_indexes.R")

plotTemplate <- function (MDI, axisx=TRUE, axisy=TRUE) {
  # clean template
  plot(MDI,ylim=c(0.2,1),xlim= c(0,71),axes = FALSE,ann = FALSE,type = 'n')

  # Chance and criterium gray lines
  abline(h = 0.5, untf = FALSE, col=rgb(192,192,192,230,maxColorValue = 255))
  abline(h = 0.85, untf = FALSE, col=rgb(192,192,192,230,maxColorValue = 255))

  # connected white points template
  lines(MDI, lty= 1, lwd= 1, type = 'l')
  points(MDI, pch= 21, bg="white", cex=1.0)

  if (axisy) { # Draw the y axis
    axis(2, at=c(0.2,0.5,0.85,1), tick=TRUE, las=2, cex.axis=0.8)
  }

  if (axisx) { # Draw the x axis
    axis(1, at= c(1,20,40,60,71), tick = TRUE, las=0, cex.axis=0.8) 
  } 
}

conditionalPoints <- function (stage) {
  # conditional (black, gray) points
  from <- 1
  to <- 4
  for (i in 1:(length(stage$MDI))){
    if((sum(stage$DI[from:to] >= 0.85)) == 4){
      points(i,stage$MDI[i], pch= 21, bg="black", cex=1.0)
    } else {
      if((sum(stage$DI[from:to] >= 0.75)) == 4){
        points(i,stage$MDI[i], pch=21, bg="gray", cex=1.0)
      }
    }
    from <- from + 4
    to <- to + 4
  }
}

#save original par configs
#opar <- par( no.readonly = TRUE )

#new device
switch(
  Sys.info()[['sysname']],
  Windows = {
    #windows(record=TRUE)
    windows(7,5)
  },
  Linux   = {
    X11(width = 14, height = 5)
  },
  Darwin  = {
    quartz(7,5)
  }
)

#set two rows and four columns
split.screen( figs = c( 2, 4 ) )

# mar = c(bottom, left, top, right)
# par(mar = c(0, 4, 4, 0))

##################################  Bongo  ###################################
##############################################################################

# row.column(1.1)
screen(1)
par(mar = c(0, 4, 4, 0))

plotTemplate(Bongo$stageA1$MDI, FALSE)
conditionalPoints(Bongo$stageA1)

legend("bottomright", expression(A^{1} ~ "- Symmetrical "), cex=0.8)
text(x=10, y=0.7, labels="Bongo", font=2, col="black", cex=.8) 

# row.column(1.2)
screen(2)
par(mar = c(0, 0, 4, 0))

plotTemplate(Bongo$stageB1$MDI, FALSE, FALSE)
conditionalPoints(Bongo$stageB1)

legend("bottomright", expression(B^{1} ~ "- Nonsymmetrical "), cex=0.8)

# row.column(1.3) - Gap
#screen(3)

# row.column(1.4) - Gap
#screen(4)

##################################  Negão  ###################################
##############################################################################

# row.column(2.1)
screen(5)
par(mar=c(4, 4, 0, 0))

plotTemplate(Negao$stageA1$MDI)
conditionalPoints(Negao$stageA1)

legend("bottomright", expression(A^{1} ~ "- Symmetrical "), cex=0.8)
text(x=10, y=0.7, labels="Negão", font=2, col="black", cex=.8) 

# row.column(2.2)
screen(6)
par(mar=c(4, 0, 0, 0))

plotTemplate(Negao$stageB1$MDI, axisy=FALSE)
conditionalPoints(Negao$stageB1)

legend("bottomright", expression(B^{1} ~ "- Nonsymmetrical "), cex=0.8)

# row.column(2.3)
screen(7)
par(mar = c(4, 0, 0, 0))

plotTemplate(Negao$stageA2$MDI, axisy=FALSE)
conditionalPoints(Negao$stageA2)

legend("bottomright", expression(A^{2} ~ "- Symmetrical "), cex=0.8)

# row.column(2.4)
screen(8)
par(mar = c(4, 0, 0, 4))

plotTemplate(Negao$stageB2$MDI, axisy=FALSE)
conditionalPoints(Negao$stageB2)

legend("bottomright", expression(B^{2} ~ "- Nonsymmetrical "),
  #ncol=2,
  #lty=c(1,3),
  #lwd=c(2,1),
  #pch= c(0,21),
  #bty="b",
  cex=0.8
)

#reset
close.screen( all = TRUE )

title(
  #main = c("Mean Discriminative index"), 
  xlab = "Sessions",
  ylab = "Discriminative Index",
  cex.main = 1.2, cex.lab = 1.0, font.main = 1, font.lab = 1
)
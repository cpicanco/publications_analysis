source("discriminative_indexes.R")

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
    X11(width = 7, height = 5)
  },
  Darwin  = {
    quartz(7,5)
  }
)

#set to rows
split.screen( figs = c( 2, 2 ) )

#fixed first (1.1)
screen(1)

#top - c(bottom, left, top, right)
par(mar = c(0, 4, 4, 0))

plot(Negao$stageA1$MDI,ylim=c(0.2,1),xlim= c(0,71),axes = FALSE,ann = FALSE,type = 'n')

#Chance and criterium gray lines
abline(h = 0.5, untf = FALSE, col=rgb(192,192,192,230,maxColorValue = 255))
abline(h = 0.85, untf = FALSE, col=rgb(192,192,192,230,maxColorValue = 255))

lines(Negao$stageA1$MDI, lty= 1, lwd= 1, type = 'l')

points(Negao$stageA1$MDI, pch= 21, bg="white", cex=1.0)

# Draw the y axis
axis(2, at= c(0.2,0.5,0.85,1), tick = TRUE, las=2, cex.axis=0.8) 

#Conditional colored (black) points
from <- 1
to <- 4
for (i in 1:(length(Negao$stageA1$MDI))){
  if((sum(Negao$stageA1$DI[from:to] >= 0.85)) == 4){
    points(i,Negao$stageA1$MDI[i], pch= 21, bg="black", cex=1.0)
  } else {
    if((sum(Negao$stageA1$DI[from:to] >= 0.75)) == 4){
      points(i,Negao$stageA1$MDI[i], pch=21, bg="gray", cex=1.0)
    }
  }
  from <- from + 4
  to <- to + 4
}

legend(
  "bottomright",
  expression(A^{1} ~ "- Symmetrical"),
  #ncol=2,
  #lty=c(1,3),
  #lwd=c(2,1),
  #pch= c(0,21),
  #bty="b"
  cex=0.8,
)

#fixed first (1.1)
screen(2)

#top - c(bottom, left, top, right)
par(mar = c(0, 0, 4, 4))

plot(Negao$stageB1$MDI, ylim=c(0.2,1), xlim= c(0,71), axes = FALSE, ann = FALSE, type = 'n')

#Chance and criterium gray lines
abline(h = 0.5, untf = FALSE, col=rgb(192,192,192,230,maxColorValue = 255))
abline(h = 0.85, untf = FALSE, col=rgb(192,192,192,230,maxColorValue = 255))

lines(Negao$stageB1$MDI, lty= 1, lwd= 1, type = 'l')

points(Negao$stageB1$MDI, pch= 21, bg="white", cex=1.0)

#Conditional colored (black) points
from <- 1
to <- 4
for (i in 1:(length(Negao$stageB1$MDI))){
  if((sum(Negao$stageB1$DI[from:to] >= 0.85)) == 4){
    points(i,Negao$stageB1$MDI[i],pch= 21,bg= "black", cex=1.0)
  } else {
    if((sum(Negao$stageB1$DI[from:to] >= 0.75)) == 4){
      points(i,Negao$stageB1$MDI[i],pch= 21,bg= "gray", cex=1.0)
    }
  }
  from <- from + 4
  to <- to + 4
}

legend(
  "bottomright",
  expression(B^{1} ~ "- Nonsymmetrical"),
  #ncol=2,
  #lty=c(1,3),
  #lwd=c(2,1),
  #pch= c(0,21),
  #bty="b",
  cex=0.8
)

#fix second (2.1)
screen(3)

#bottom - c(bottom, left, top, right)
par(mar = c(4, 4, 0, 0))

plot(Negao$stageA2$MDI, ylim=c(0.2,1), xlim= c(0,71), axes = FALSE, ann = FALSE, type = 'n')

#Chance and criterium gray lines
abline(h = 0.5, untf = FALSE, col=rgb(192,192,192,230,maxColorValue = 255))
abline(h = 0.85, untf = FALSE, col=rgb(192,192,192,230,maxColorValue = 255))

lines(Negao$stageA2$MDI, lty= 1, lwd= 1, type = 'l')

points(Negao$stageA2$MDI, pch= 21, bg="white", cex=1.0)

# Draw the x axis
axis(1, at= c(1,20,40,60,71), tick = TRUE, las=0, cex.axis=0.8) 

# Draw the y axis
axis(2, at= c(0.2,0.5,0.85,1), tick = TRUE, las=2, cex.axis=0.8)     

#Conditional colored (black) points
from <- 1
to <- 4
for (i in 1:(length(Negao$stageA2$MDI))){
  if((sum(Negao$stageA2$DI[from:to] >= 0.85)) == 4){
    points(i,Negao$stageA2$MDI[i],pch= 21,bg= "black", cex=1.0)
  } else {
    if((sum(Negao$stageA2$DI[from:to] >= 0.75)) == 4){
      points(i,Negao$stageA2$MDI[i],pch= 21,bg= "gray", cex=1.0)
    } 
  }
  from <- from + 4
  to <- to + 4
}

legend(
  "bottomright",
  expression(A^{2} ~ "- Symmetrical"),
  #ncol=2,
  #lty=c(1,3),
  #lwd=c(2,1),
  #pch= c(0,21),
  #bty="b",
  cex=0.8
)

#fixed forth (2.2)
screen(4)

#bottom - c(bottom, left, top, right)
par(mar = c(4, 0, 0, 4))

plot(Negao$stageB2$MDI, ylim=c(0.2,1), xlim= c(0,71), axes = FALSE, ann = FALSE, type = 'n')

#Chance and criterium gray lines
abline(h = 0.5, untf = FALSE, col=rgb(192,192,192,230,maxColorValue = 255))
abline(h = 0.85, untf = FALSE, col=rgb(192,192,192,230,maxColorValue = 255))

lines(Negao$stageB2$MDI, lty= 1, lwd= 1, type = 'l')

points(Negao$stageB2$MDI, pch= 21, bg="white", cex=1.0)

# Draw the x axis
axis(1, at= c(1,20,40,60,71), tick = TRUE, las=0, cex.axis=0.8)    

#Conditional colored (black) points
from <- 1
to <- 4
for (i in 1:(length(Negao$stageB2$MDI))){
  if((sum(Negao$stageB2$DI[from:to] >= 0.85)) == 4){
    points(i, Negao$stageB2$MDI[i], pch= 21, bg="black", cex=1.0)
  } else {
    if((sum(Negao$stageB2$DI[from:to] >= 0.75)) == 4){
      points(i, Negao$stageB2$MDI[i], pch= 21, bg="gray", cex=1.0)
    }
  }
  from <- from + 4
  to <- to + 4
}

legend(
  "bottomright",
  expression(B^{2} ~ "- Nonsymmetrical"),
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
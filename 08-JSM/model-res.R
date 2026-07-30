wd <- getwd()
setwd("~/Documents/ISU/ISU Statistics/Stat601/Hwk1")

source("../Rfunctions/newtraph.txt")
source("../Rfunctions/gammaders.txt")

x <- read.table("../Data/gammadat.txt")
setwd(wd)

names(x) <- c("a", "b")
summary(x)

par(mfrow=c(2,1))
hist(x$a, breaks=seq(0, 7, .25), prob=T)
hist(x$b, breaks=seq(0, 7, .25), prob=T)

res1 <- newtraph(gammaders, x$a, c(2,.2)) # full model pt 1
res2 <- newtraph(gammaders, x$b, c(2,1))

resN <- newtraph(gammaders, c(x$a,x$b), c(2,1))

C <- matrix(c(1,0,0,1), byrow=TRUE, nrow=2)
crit <- qnorm(.95)
res1CI <- crit*sqrt(res1[[3]])
res1[[1]][1] +c(-1,1)*res1CI[1,1] # (4.212, 8.186)
res1[[1]][2] +c(-1,1)*res1CI[2,2] # (1.303, 2.609)

res2CI <- crit*sqrt(res2[[3]])
res2[[1]][1] +c(-1,1)*res2CI[1,1] # (6.163, 12.047)
res2[[1]][2] +c(-1,1)*res2CI[2,2] # (2.017, 4.025)

resNCI <- crit*sqrt(resN[[3]])
resN[[1]][1] +c(-1,1)*resNCI[1,1] # (5.668, 9.007)
resN[[1]][2] +c(-1,1)*resNCI[2,2] # (1.814, 2.932)


LF <- res1[[2]] + res2[[2]]
LR <- resN[[2]]
t <- 2*(LF-LR)
p <- pchisq(t,2) # conculsion: No evidence that the two distributions are different...



xg <- seq(0, 7,.001)
pg1 <- dgamma(xg, shape=res1[[1]][1], rate=res1[[1]][2])
pg2 <- dgamma(xg, shape=res2[[1]][1], rate=res2[[1]][2])
pgN <- dgamma(xg, shape=resN[[1]][1], rate=resN[[1]][2])

par1 <- res1[[1]]
par2 <- res2[[1]]
hist(x$a, breaks=seq(0,7,.25), prob=T, main=NULL)
title(main=substitute(paste("GammaData, with Distribution Gamma(", list(x,y), ")", sep=""), list(x=round(par1[1],2), y=round(par1[2],2))))
lines(xg, pg1)

hist(x$b, breaks=seq(0, 7, .25), prob=T, main=NULL)
title(main=substitute(paste("GammaData, with Distribution Gamma(", list(x,y), ")", sep=""), list(x=round(par2[1],2), y=round(par2[2],2))))
lines(xg, pg2)

parN <- resN[[1]]

library(ggplot2)
library(tidyr)
dens <- tibble(type = rep(c("Combined", "Group 1 alone", "Group 2 alone"), 
                          each = 7001), 
               dens = c(pgN, pg1, pg2),
               x = rep(xg, times = 3))

xlong <- x |> pivot_longer(a:b, names_to="sample") 

ggplot() + 
  geom_histogram(data = xlong, aes(x = value, y = after_stat(density)), breaks=seq(0, 7, .25), fill = "grey90", color = "grey50") +
  geom_line(data = dens, aes(x = x, y = dens, color = type, linetype = type), inherit.aes=F, linewidth = 1) + 
  theme_bw() + 
  scale_color_manual(values = c("#0F2439", "#8e4506", "#1a5c7f")) +
  theme(legend.position.inside = c(1,1), legend.justification = c(1.01,1.01), 
        legend.position = "inside", legend.background = element_rect(fill="transparent"),
        legend.title=element_blank(), axis.title = element_blank())


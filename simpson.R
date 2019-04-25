library(tidyverse)

theme_set(
        theme_bw() +
                theme(legend.position = "top", 
                      axis.text = element_text(size = 11), 
                      axis.title = element_text(size = 12, face = "bold"))
)

x1 <- c(1,2,3,4)
y1 <- x1 + 5

x2 <- x1 + 7
y2 <- x2 - 7

x <- c(x1,x2)
y <- c(y1,y2)

par(las=1)
par(mar=c(4, 4, 0.5, 0.5) + 0.1)
par(mgp=c(2,1,0))

png("img/simpson.png", res = 600, width = 14, height = 13, units = "cm")

plot(x,y, cex=2, pch=21,
     col=rep(c("blue", "red"), each=4), bg=rep(c("lightblue", "pink"), each=4),
     xlim=range(x)+c(-2,2), ylim=range(y)+c(-2,2))
abline(lm(y1 ~ x1), col="blue", lwd=2)
abline(lm(y2 ~ x2), col="red", lwd=2)
abline(lm(y  ~ x), lwd=2, lty=2)

dev.off()


treecov <- c(10,	30,	20,	30,	40,	60,	30,	30,	5,	5,	90,	60,	60,	70,	80,	85,	50,	80)
richn <- c(5,	8,	4,	5,	12,	4,	10,	11,	11,	7,	6,	9,	10,	9,	10,	8,	7,	8)
cover <-c(7100,	10200,	2700,	5000,	4800,	3300,	7900,	9500,	9900,	5800,	7700,	6800,	6500,	5100,	7400,	5300,	5400,	4400)
treetyp <-c(1,	1,	1,	1,	1,	1,	1,	1,	0,	0,	2,	2,	2,	2,	2,	2,	2,	2)
treespec <-c(3,	3,	3,	1,	2,	2,	2,	2,	4,	4,	5,	5,	5,	6,	6,	6,	6,	6)
dev.off()

plot(#treecov,
  richn)


library(ggplot2)
qplot(treecov,treespec)

lm <-lm(treecov~treespec)
plot(lm)
t.test(treecov,treespec)
shapiro.test(treetyp)

qqnorm(richn)
cor.test(treetyp,treespec,method = "pearson")

cor.test(treecov,richn)

cor.test(treetyp,cover)
cor.test(treetyp,richn)

cor.test(treespec,cover)
cor.test(treespec,richn)

corstat <- function (x) {
  t.test(x)
  
}

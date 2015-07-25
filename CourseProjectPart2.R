library(lattice)
data(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)
table(ToothGrowth$supp,ToothGrowth$dose)
bwplot(ToothGrowth$len ~ToothGrowth$supp | ToothGrowth$dose)
aggregate(ToothGrowth$len,list(ToothGrowth$dose,ToothGrowth$supp)
          ,FUN=function(x) c(x_mean = mean(x), x_sd = sd(x)))
t.test(len ~ supp, data = ToothGrowth)

d5 <- ToothGrowth[which(ToothGrowth$dose==.5),1]
d10 <- ToothGrowth[which(ToothGrowth$dose==1),1]
d20 <- ToothGrowth[which(ToothGrowth$dose==2),1]

d510_t1 <- t.test(d5, d10, paired=FALSE, var.equal=TRUE)
d510_t2 <- t.test(d5, d10, paired=FALSE, var.equal=FALSE)
d510 <- data.frame("p-value"=c(d510_t1$p.value, d510_t2$p.value),
                          "Conf-Low"=c(d510_t1$conf[1],d510_t2$conf[1]),
                          "Conf-High"=c(d510_t1$conf[2],d510_t2$conf[2]),
                           row.names=c("t1","t2"), "Dose"="[0.5..1]")

d520_t1 <- t.test(d5, d20, paired=FALSE, var.equal=TRUE)
d520_t2 <- t.test(d5, d20, paired=FALSE, var.equal=FALSE)
d520 <- data.frame("p-value"=c(d520_t1$p.value, d520_t2$p.value),
                            "Conf-Low"=c(d520_t1$conf[1],d520_t2$conf[1]),
                            "Conf-High"=c(d520_t1$conf[2],d520_t2$conf[2]), 
                            row.names=c("t1","t2"), "Dose"="[0.5..2]")

d1020_t1 <- t.test(d10, d20, paired=FALSE, var.equal=TRUE)
d1020_t2 <- t.test(d10, d20, paired=FALSE, var.equal=FALSE)
d1020 <- data.frame("p-value"=c(d1020_t1$p.value, d1020_t2$p.value),
                           "Conf-Low"=c(d1020_t1$conf[1],d1020_t2$conf[1]),
                           "Conf-High"=c(d1020_t1$conf[2],d1020_t2$conf[2]), 
                           row.names=c("t1","t2"), "Dose"="[1..2]")
doseTot <- rbind(d510,d520,d1020)
doseTot
# Dinámicas ocultas y comportamientos disfuncionales en el trabajo en gupo. 2021

# General
n118 <- read.delim("n118.csv")
n118.CGrade.Year<-n118[,c("Grade","Year")]
boxplot(Grade~Year,n118.CGrade.Year)

# Estudio segmentado por años

for (year in sort(unique(n118$Year))) {
	sum <-summary(n118.CGrade.Year[n118.CGrade.Year$Year==year,]);
	cat(year, " ", sum[1], " ", sum[3], " ", sum[4], " ", sum[6], "\n")
}

# cat("Min & Median & Mean &Max \\\\")
# for (year in sort(unique(n118$Year))) {
#   sum <-summary(n118.CGrade.Year[n118.CGrade.Year$Year==year,]);
#   cat(year, " & ", strsplit(sum[1], ":")[[1]][2],
#       " & ", strsplit(sum[3], ":")[[1]][2],
#       " & ", strsplit(sum[4], ":")[[1]][2],
#       " & ", strsplit(sum[6], ":")[[1]][2], "\\\\ \n")
# }

# Tests de independencia sobre el año
n118.CGrade.Year.lm <- lm(Grade~Year, n118.CGrade.Year)
n118.CGrade.Year.aov <- aov( n118.CGrade.Year.lm)
summary(n118.CGrade.Year.aov)
n118.CGrade.Year.kw <- kruskal.test(Grade~Year, n118.CGrade.Year)
show(n118.CGrade.Year.kw)
n118.CGrade.Year.tukey <- TukeyHSD(n118.CGrade.Year.aov)
show(n118.CGrade.Year.tukey)


# Tests de independencia sobre el tipo de líder
n118.CGrade.Leader <-n118[,c("Grade","Leader")]
n118.CGrade.Leader.boxplot <-boxplot(Grade~Leader, n118.CGrade.Leader)
show(n118.CGrade.Leader.boxplot$out)
n118.NO.CGrade.Leader<- +
  n118.CGrade.Leader[!(n118.CGrade.Leader$Grade %in% +
                         n118.CGrade.Leader.boxplot$out),]
summary(n118.NO.CGrade.Leader)

for (leader in sort(unique(n118$Leader))) {
  sum <-summary(n118.CGrade.Leader[n118.CGrade.Leader$Leader==leader,]);
#  cat(leader, " ", sum[1], " ", sum[3], " ", sum[4], " ", sum[6], "\n")
     cat(leader, " & ", strsplit(sum[1], ":")[[1]][2],
         " & ", strsplit(sum[3], ":")[[1]][2],
         " & ", strsplit(sum[4], ":")[[1]][2],
         " & ", strsplit(sum[6], ":")[[1]][2], "\\\\ \n")
}

n118.NO.CGrade.Leader.lm <- lm(Grade~Leader,n118.NO.CGrade.Leader)
n118.NO.CGrade.Leader.aov<-aov(n118.NO.CGrade.Leader.lm)
summary(n118.NO.CGrade.Leader.aov)
#Df Sum Sq Mean Sq F value Pr(>F)
#Leader        2    7.7   3.852   3.385 0.0376 *
#  Residuals   106  120.6   1.138
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
qf(1-0.95,2,106,lower.tail = F)
#[1] 3.082015

n118.NO.CGrade.Leader.kw <-kruskal.test(Grade~Leader, n118.CGrade.Leader.boxplot)
#Kruskal-Wallis rank sum test
#data:  Grade by Leader
#Kruskal-Wallis chi-squared = 6.6899, df = 2, p-value = 0.03526
n118.NO.CGrade.Leader.tukey <- TukeyHSD(n118.NO.CGrade.Leader.aov)
#Tukey multiple comparisons of means
#95% family-wise confidence level
#Fit: aov(formula = n118.NO.CGrades.Leader.lm)
#$Leader
#diff        lwr        upr     p adj
#D-A  0.02367742 -0.6579991 0.70535390 0.9962488
#L-A -0.52107730 -1.0944751 0.05232049 0.0829032
#L-D -0.54475472 -1.1600371 0.07052771 0.0937293

n118.NO.CGrade.LeaderAD <- n118.NO.CGrade.Leader[n118.NO.CGrade.Leader$Leader == "D" |n118.NO.CGrade.Leader$Leader =="A",]
n118.NO.CGrade.LeaderAD.lm <- lm(Grade~Leader,n118.NO.CGrade.LeaderAD)
t.test(Grade~Leader,n118.NO.CGrade.LeaderAD)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
#
# Fit: aov(formula = n118.NO.CGrades.Leader.lm)
#
# $Leader
# diff        lwr        upr     p adj
# D-A  0.02367742 -0.6579991 0.70535390 0.9962488
# L-A -0.52107730 -1.0944751 0.05232049 0.0829032
# L-D -0.54475472 -1.1600371 0.07052771 0.0937293


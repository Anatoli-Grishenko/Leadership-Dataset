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

n118.CGrade.Year.plot <- ggplot(n118.Grades.Year.melt, aes(x=Grade, y=Count, group=Year, colour=Year))
n118.Grades.Year.plot+geom_line()

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
FullScrumADL <- read.csv("FullScrumADL.csv", row.names=1)
FullScrumADL.lm<-lm(EC~Leader,FullScrumADL)
FullScrumADL.aov <- aov(FullScrumADL.lm)
summary(FullScrumADL.aov)
# Df Sum Sq Mean Sq F value Pr(>F)
# Leader         2   4.89  2.4437   107.5 <2e-16 ***
#   Residuals   6133 139.36  0.0227
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
FullScrumADL.tukey <- TukeyHSD(FullScrumADL.aov)
FullScrumADL.tukey
# Tukey multiple comparisons of means
# 95% family-wise confidence level
#
# Fit: aov(formula = FullScrumADL.lm)
#
# $Leader
# diff          lwr         upr     p adj
# D-A  0.07278430  0.060193214  0.08537538 0.0000000
# L-A  0.01181177  0.001092603  0.02253094 0.0265295
# L-D -0.06097253 -0.072281607 -0.04966345 0.0000000
plot(FullScrumADL.tukey)
FullScrumADL.boxplot <- boxplot(EC~Leader,FullScrumADL)
FullScrumADL.boxplot$out
# [1]  0.479  0.499  0.519  0.538  0.515  0.534  0.554  0.573  0.528  0.508  0.528 -0.315 -0.295  0.482 -0.490 -0.470 -0.472
# [18] -0.463 -0.444 -0.424 -0.415 -0.396 -0.376 -0.356 -0.337 -0.317 -0.297  0.472  0.477  0.478  0.469  0.488  0.479  0.499
# [35]  0.605  0.624  0.626  0.646  0.665  0.602  0.621  0.593  0.613  0.632  0.591  0.611  0.612  0.632  0.590  0.500  0.519
# [52]  0.539  0.534  0.553  0.573  0.593  0.550  0.570  0.590  0.572  0.592  0.611  0.613  0.632  0.652  0.671  0.691  0.485
# [69]  0.505  0.525  0.492  0.511  0.484  0.504  0.524  0.491 -0.291 -0.395 -0.375 -0.355 -0.336 -0.316 -0.297  0.500  0.519
# [86]  0.539  0.505  0.525  0.545  0.564  0.573  0.593  0.487  0.507  0.526  0.546  0.565  0.585  0.488  0.508  0.528  0.547
# [103]  0.567  0.495  0.514  0.534
FullScrumADL.NO <- FullScrumADL[!(FullScrumADL$EC %in% FullScrumADL.boxplot$out),]
summary(FullScrumADL)
# Leader                EC
# Length:6136        Min.   :-0.4900
# Class :character   1st Qu.: 0.0060
# Mode  :character   Median : 0.0940
# Mean   : 0.1147
# 3rd Qu.: 0.2040
# Max.   : 0.6910
summary(FullScrumADL.NO)
# Leader                EC
# Length:6008        Min.   :-0.278
# Class :character   1st Qu.: 0.005
# Mode  :character   Median : 0.091
# Mean   : 0.109
# 3rd Qu.: 0.197
# Max.   : 0.582
FullScrumADL.NO.lm<-lm(EC~Leader,FullScrumADL.NO)
FullScrumADL.NO.aov<-aov(FullScrumADL.NO.lm)
summary(FullScrumADL.NO.aov)
# Df Sum Sq Mean Sq F value Pr(>F)
# Leader         2   4.04  2.0201   105.4 <2e-16 ***
#   Residuals   6005 115.14  0.0192
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
FullScrumADL.NO.tukey <-TukeyHSD (FullScrumADL.NO.aov)
FullScrumADL.NO.tukey
# Tukey multiple comparisons of means
# 95% family-wise confidence level
#
# Fit: aov(formula = FullScrumADL.NO.lm)
#
# $Leader
# diff          lwr         upr     p adj
# D-A  0.064020926  0.052328961  0.07571289 0.0000000
# L-A  0.005111773 -0.004835529  0.01505908 0.4503998
# L-D -0.058909153 -0.069413993 -0.04840431 0.0000000
plot(FullScrumADL.NO.tukey)

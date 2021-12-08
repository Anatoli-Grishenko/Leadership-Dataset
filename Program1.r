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


install.packages("BSDA")
library(BSDA)
# Exerises 1.1 and 1.4
a = read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter1/DATA for Exercises 1.1 and 1.4.csv",h=TRUE)
SIGN.test(a$two_week, a$start, md = 0, alternative = "greater")
wilcox.test(a$two_week, a$start, paired=TRUE, alternative="greater")

# Exerises 1.2 and 1.5
b = read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter1/DATA for Exercises 1.2 and 1.5.csv",h=TRUE)
SIGN.test(b$intervention, b$control, alternative = "greater")
wilcox.test(b$intervention,b$control, paired=TRUE, alternative="greater")

# Exerises 1.3 and 1.6
c = read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter1/DATA for Exercises 1.3 and 1.6.csv",h=TRUE)
SIGN.test(c$price1, c$price2, md = 0, alternative = "two.sided")
wilcox.test(c$price1, c$price2, paired=TRUE, alternative="two.sided")

# Exerises 1.7
d = read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter1/DATA for Exercise 1.7.csv",h=TRUE)
x = subset(d, d$group=="Cx")
Cx = x[["nstay"]]
y = subset(d, d$group=="Tx")
Tx = y[["nstay"]]
wilcox.test(Cx,Tx, paired=FALSE, alternative="greater")


# Exerises 1.8
e = read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter1/DATA for Exercise 1.8.csv",h=TRUE)
x = subset(e, e$group=="Adult")
Adult = x[["correct"]]
y = subset(e, e$group=="Adolescent")
Adolescent = y[["correct"]]
wilcox.test(Adolescent, Adult, paired=FALSE, alternative="less")

# Exerises 1.9
f = read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter1/DATA for Exercise 1.9.csv",h=TRUE)
x = subset(f, f$gender=="Male")
Male = x[["nlikes"]]
y = subset(f, f$gender=="Female")
Female = y[["nlikes"]]
wilcox.test(Female,Male, paired=FALSE, alternative="two.sided")

# Exerises 1.10
g = read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter1/DATA for Exercise 1.10.csv",h=TRUE)
x1 = subset(g, g$status=="work")
work= x1[["nexercise"]]
y1 = subset(g, g$status=="home")
home = y1[["nexercise"]]
wilcox.test(work,home, paired=FALSE, alternative="two.sided")
wilcox.test(work,home, paired=FALSE, alternative="less")
ansari.test(work,home, alternative="two.sided")
ansari.test(work,home, alternative="less")

# Exerises 1.11 and 1.14.csv
h = read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter1/DATA for Exercises 1.11 and 1.14.csv",h=TRUE)
x = subset(h, h$scale=="UCLA")
UCLA= x[["score"]]
y = subset(h, h$scale=="Constant")
Constant = y[["score"]]
wilcox.test(Constant, UCLA, paired=FALSE, alternative="two.sided")
ansari.test(Constant, UCLA, alternative="greater")
ks.test(Constant, UCLA, paired=FALSE, alternative="two.sided")

# Exerises 1.12
i = read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter1/DATA for Exercise 1.12.csv",h=TRUE)
x2 = subset(i, i$group=="Tx")
Tx= x2[["score"]]
y2 = subset(i, i$group=="Cx")
Cx = y2[["score"]]
ks.test(Tx, Cx, alternative="greater")

# Exerises 1.13
j = read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter1/DATA for Exercise 1.13.csv",h=TRUE)
x = subset(j, j$group=="Tx")
Tx= x[["reduction"]]
y = subset(j, j$group=="Cx")
Cx = y[["reduction"]]
ks.test(Tx,Cx, alternative="less")



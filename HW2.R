install.packages("BSDA")
library(BSDA)

# Exerices 2.1

a <- read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter2/DATA for Exercise 2.1.csv",h=TRUE)
response <- matrix(c(a$Response),nrow=7, byrow=TRUE)
friedman.test(response)

# Exerices 2.2

b <- read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter2/DATA for Exercise 2.2.csv",h=TRUE)
Probability <- matrix(c(b$Probability),nrow=8, byrow=TRUE)
friedman.test(Probability)
#subset data
text<- b[b$Contact=="text  ",]
letter<- b[b$Contact=="letter",]
phone <- subset(b, Contact == "phone ")
wilcox.test(letter$Probability, phone$Probability, paired=TRUE, alternative="two.sided")
wilcox.test(letter$Probability, text$Probability, paired=TRUE, alternative="two.sided")
wilcox.test(phone$Probability, text$Probability, paired=TRUE, alternative="two.sided")

# Exerices 2.3

c = read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter2/DATA for Exercise 2.3.csv",h=TRUE)
#subsetting data to get pond A, B, c as a variable. 
PondA <- c[c$Pond=="A",]
PondB <- c[c$Pond=="B",]
PondC <- c[c$Pond=="C",]
Ponds <- list(PondA$Lead, PondB$Lead, PondC$Lead)
kruskal.test(Ponds)
wilcox.test(PondA$Lead, PondB$Lead, alternative="two.sided")
wilcox.test(PondC$Lead, PondA$Lead, alternative="two.sided")
wilcox.test(PondB$Lead, PondC$Lead, alternative="two.sided")

# Exercise 2.4
d <-read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter2/DATA for Exercise 2.4.csv",h=TRUE)
# tf = 24 te = 28 tt = 31 ts = 36
tf <- d[d$Temperature==24,]
te <- d[d$Temperature==28,]
tt <- d[d$Temperature==32,]
ts <- d[d$Temperature==36,]
germination <- list(tf$Rate, te$Rate, tt$Rate, ts$Rate)
kruskal.test(germination)

# Exercise 3.1
e <-read.table("C:/Users/Alice/Desktop/Spring 2020/STAT 560/DAT_Data_Exercises/CHAPTER3/DATA for Exercise 3.1.dat", 
           header=FALSE)
colnames(e) <- c("gasoline","milk")
gasoline <- e$gasoline
milk <-e$milk
rho <-cor(gasoline, milk,method = "spearman")
cor.test(gasoline,milk, alternative = "two.sided", method="spearman")

# Exercise 3.2
f <-read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter3/DATA for Exercise 3.2.csv",h=TRUE, stringsAsFactors=FALSE)
  f$vio[f$violence=="never"]<-1
  f$vio[f$violence=="sometimes"]<-2
  f$vio[f$violence=="often"]<-3
  f$education[f$educ=="<HS   "]<-1
  f$education[f$educ=="HS_grad"]<-2
  f$education[f$educ=="HS+    "]<-3
  education <- c(f$education)  
  violence <- as.numeric(c(f$vio))
  cor.test(education,violence, alternative="less", method="spearman")
  
# Exercise 3.3
 g <-read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter3/DATA for Exercise 3.3.csv",h=TRUE)
 year <- c(g$year)
 points <-c(g$points)
 cor.test(year,points, method="spearman")
# Exercise 3.4
 h <-read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter3/DATA for Exercise 3.4.csv",h=TRUE)
 heart_valve<-matrix(c(h$count),nrow=4, ncol = 3, dimnames = list(age = c('20s30s', '40s50s','60s', '70splus'), Group=c('Denver','Stockholm','Washington' )))
  fisher.test(heart_valve)

# Exercise 3.5
 i <-read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter3/DATA for Exercise 3.5.csv",h=TRUE)
 middle_school<-matrix(c(i$count),nrow=3, ncol = 2, dimnames = list(activity = c('friends', 'sports','studying'), gender=c('boy','girl' )))
 fisher.test(middle_school)

 # Exercise 3.6
name <-rep("pen", 6)
time <-rep("<1900", 6)
df1 <-data.frame(name, time)
name <-rep("pen", 4)
time <-rep("1900+", 4)
df2 <-data.frame(name, time)
name <-rep("real", 9)
time <-rep("<1900", 9)
df3 <- data.frame(name, time)
name <-rep("real", 11)
time <-rep("1900+", 11)
df4<-data.frame(name, time) 
j <- rbind(df1, df2, df3, df4)
writers<-table(j$name, j$time)
writers <-matrix(writers,nrow=2, ncol = 2, dimnames = list(name = c('pen', 'real'), time=c('<1900','1990+')))
fisher.test(writers)

Writer<-matrix(c(6,9,4,11), nrow=2, dimnames=list(Name= c('Pen', 'Real', time=c('<1900','1990+'))),  byrow=FALSE)
fisher.test(Writer)

 # Exercise 3.7
 k <-read.csv("C:/Users/Alice/Desktop/Spring 2020/STAT 560/CSV_Data_Exercises/Chapter3/DATA for Exercise 3.7.csv",h=TRUE)
 tribes<-table(k$tribe, k$occupation)
 tribes<-matrix(tribes,nrow=3, ncol = 5, dimnames = list(tribes = c('A', 'B','C'), occupation=c('farming','fishing','gathering','hunting', 'trading' )))
 fisher.test(tribes)
 levels(k$tribe)
 levels(k$occupation)

 
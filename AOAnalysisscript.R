#######################################################################################
#AOAnalysisscript.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", "moments","GPArotation","nFactors",
         "boot","psy","ResearchMethods", "car","vcd", "gridExtra"), library, character.only=T)
#####################################################################################
#IMPORTING DATA
#####################################################################################

#uploading data ---------------------------------------------------------------------
#Load the data set.
#All data are stored in  http://figshare.com/articles/The_reliability_of_AO_classification_on_femur_fractures_among_orthopedic_residents/103664
#Note that the data set has been reorganized to be applied to some functions

#Functions to pull the dara from the internet file 
#see http://goo.gl/mQwxO on how to get this link
webdata <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVW3PoO2euydHFkeHVMd3FyeVlkZE1ySlc2bWEwZFE&single=true&gid=1&output=csv"
,ssl.verifypeer = FALSE)
data<-read.csv(textConnection(webdata))

webdata1 <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVW3PoO2euydHFkeHVMd3FyeVlkZE1ySlc2bWEwZFE&single=true&gid=2&output=csv
"
                  ,ssl.verifypeer = FALSE)
datamosaic<-read.csv(textConnection(webdata1))

###########################################################################################
#DATA MANAGEMENT
###########################################################################################
#Section dedicated to any data management necessary
#Reconding variables to a numerical/discrete format. This format of data was used 
#to create some of the graphical displays.

#Recoding variables
data$T1 <-as.numeric(car::recode(data$R1, "'33A1' = 1; '33A2' = 2; '33A3' = 3;
                         '33B1' = 4; '33B2' = 5; '33B3' = 6;
                         '33C1' = 7; '33C2' = 8; '33C3' = 9"))
data$T2 <-as.numeric(car::recode(data$R2, "'33A1' = 1; '33A2' = 2; '33A3' = 3;
                         '33B1' = 4; '33B2' = 5; '33B3' = 6;
                         '33C1' = 7; '33C2' = 8; '33C3' = 9"))
data$T3 <-as.numeric(car::recode(data$R3, "'33A1' = 1; '33A2' = 2; '33A3' = 3;
                         '33B1' = 4; '33B2' = 5; '33B3' = 6;
                         '33C1' = 7; '33C2' = 8; '33C3' = 9"))
attach(data)
numdata<-data.frame(T1,T2,T3) #creating a dataframe with the ne recoded variables
        
##Graph 2 - Responses pattern mosaic comparing PGY2 and PGY3 answers for each moment of aplication.
#Create a subset in the data to contain students from the 2nd year
pg2 <- subset(data,Gyear == 2)
View(pg2)

#Creating a subset in the data to contain students from the 3rd year
pg3 <- subset(data,Gyear == 3)
View(pg3)
###########################################################################################
#EXPLORATORY DATA ANALYSIS
###########################################################################################
###Section wih several exploratory data analysis functions
#Exploratory Data Anlysis
#dim(data)
#str (data)
#head(data)
#names(data)
summary(data)#This comand will provide a whole set of descriptive results for each variables
Hmisc::describe(data$R1)
Hmisc::describe(data$R2)
Hmisc::describe(data$R3)
#ad.test() # Anderson-Darling test for normality
#skewness() #Will provide skweness analysis
#kurtosis() - 3 #Will provide kurtosis analysis
#qplot() # histogram plot
#pwr.anova.test(k = , n = , f = , sig.level = , power = )#Power analysis for variance analysis
#boxplot() #will provide a boxplot for the variables to analysis potential outliers
#detach(data)
attach(data)

###########################################################################################
#OBJECTIVE 1: INTER OBSERVER AGREEMENT 
###########################################################################################
data$subjects<-c("S1","S2","S3","S4","S5","S6") #creating a vector in the dataset to classify each respondent.

#Here we are managing the dataset in order to perform INTER observer agreement analysis
stu1<-subset(data,data$subjects=="S1") #Defining subset for each respondent
stu2<-subset(data,data$subjects=="S2")
stu3<-subset(data,data$subjects=="S3")
stu4<-subset(data,data$subjects=="S4")
stu5<-subset(data,data$subjects=="S5")
stu6<-subset(data,data$subjects=="S6")

#creating a dataframe with the answers of each respondent to the AO instrument
#We are creating one dataframe for each time of application (T1,T2 and T3)
InterT1<-data.frame(stu1$R1,stu2$R1,stu3$R1,stu4$R1,stu5$R1,stu6$R1) 
kappam.fleiss(InterT1) #Kaplam-Fleiss agreement method for multiple raters
agree(InterT1, tolerance=0) #Percent of agrement

InterT2<-data.frame(stu1$R2,stu2$R2,stu3$R2,stu4$R2,stu5$R2,stu6$R2)
kappam.fleiss(InterT2)
agree(InterT2, tolerance=0)

InterT3<-data.frame(stu1$R3,stu2$R3,stu3$R3,stu4$R3,stu5$R3,stu6$R3)
kappam.fleiss(InterT3)
agree(InterT3, tolerance=0)

##Graph 1 - Inter-observer agreement for each moment of application

#Creating dataframe to populate the graph
Interdotgraph<-data.frame(T1,T2,T3,data$subjects, data$Case)

#Creating ggplot object
LineplotT1<-ggplot(Interdotgraph, aes(data.subjects, T1, group=data.Case)) + 
  geom_point(aes(colour = data.Case, shape = data.Case), size=6) +
  geom_line(aes(colour=data.Case)) + theme_bw() +
  ylab("Before Ao Training") + xlab("Respondents")+
  scale_y_discrete(breaks=c("1", "2","3","4","5","6","7","8"), 
  labels=c("33A1", "33A2","33A3","33B1","33B2","33B3","33C1","33C2"))

LineplotT2<-ggplot(Interdotgraph, aes(data.subjects, T2, group=data.Case)) + 
  geom_point(aes(colour = data.Case, shape = data.Case), size=6) +
  geom_line(aes(colour=data.Case)) + theme_bw() +
  ylab("After Ao Training") + xlab("Respondents")+
  scale_y_discrete(breaks=c("1", "2","3","4","5","6","7","8"), 
                   labels=c("33A1", "33A2","33A3","33B1","33B2","33B3","33C1","33C2"))

LineplotT3<-ggplot(Interdotgraph, aes(data.subjects, T3, group=data.Case)) + 
  geom_point(aes(colour = data.Case, shape = data.Case), size=6) +
  geom_line(aes(colour=data.Case)) + theme_bw() +
  ylab("1 Month After Ao Training") + xlab("Respondents")+
  scale_y_discrete(breaks=c("1", "2","3","4","5","6","7","8"), 
                   labels=c("33A1", "33A2","33A3","33B1","33B2","33B3","33C1","33C2"))

#Arrange ggplot objects in the same plot area
grid.arrange(LineplotT1,LineplotT2,LineplotT3,nrow=3,ncol=1)

#Create mosaic plot
mosaic(Year ~ Responses | Time, data = datamosaic,highlighting_direction = "right")

###########################################################################################
#TABLE 2: INTRA OBSERVER AGREEMENT 
###########################################################################################
#Creating data frames for agreement between times of application for PGY2 (T1, T2 and T3)
Inter1<-data.frame(R1,R2) #Agreement between Before and After AO Training
agree(Inter1, tolerance=0)
obj1<-ckappa(Inter1)

Inter2<-data.frame(R1,R3) #Agreement between Before and After 1 month of AO training
agree(Inter2, tolerance=0)
obj2<-ckappa(Inter2)

Inter3<-data.frame(R2,R3) #Agreement between After and After 1 month of AO training
agree(Inter3, tolerance=0)
obj3<-ckappa(Inter3)
        
#Graph 3 - Agreement of responses for moments of application (T1, T2, T3)
agreementplot(t(obj1$table), main = "Agreement between Before and After AO Training
              ",xlab_rot=90, ylab_rot=0, 
              xlab_just="right", ylab_just="right")
agreementplot(t(obj2$table), main = "Agreement between Before and 
              After 1 month of AO training
              ",
              xlab_rot=90, ylab_rot=0, 
              xlab_just="right", ylab_just="right")
agreementplot(t(obj3$table), main = "Agreement between After and 
              After 1 month of AO training
              ",
              xlab_rot=90, ylab_rot=0, 
              xlab_just="right", ylab_just="right")

#Graph 4 - Path of responses per student for the three moments of application
par(mar=c(4,4,1,1)+0.1)
parcoord(numdata, main=
           "Path of responses in T1, T2 and T2")
text(locator(), "33A1", 2)
text(locator(), "33A2", 2)
text(locator(), "33A3", 2)
text(locator(), "33B1", 2)
text(locator(), "33B2", 2)
text(locator(), "33B3", 2)
text(locator(), "33C1", 2)
text(locator(), "33C2", 2)
text(locator(), "33C3", 2)

#Graph 5 - Comparison of the difference of responses between moments of application

###T1xT2
  x <- T1 #definig variables for the function
  y <- T2
  d <- Case
  diff <- data.frame(x-y)
  diff$colour[diff$x...y>=2]<-"red" #creating color vector
  diff$colour[diff$x...y<= -2]<-"red"
  diff$colour[diff$x...y<2 & diff$x...y>0] <-"blue"
  diff$colour[diff$x...y> -2 & diff$x...y<0] <-"blue"
  diff$colour[diff$x...y==0] <-"darkgreen"
  
  dotchart(diff$x...y, groups=d,pch=25,ylim=c(-6,6),xlab="Difference AO (Before and After)",
           color=diff$colour) #calling plot
  abline(abline(v=0-c(-1,0,1),lty=5, bg=2)) #drawing abline

###T1xT3
x <- T1 #definig variables for the function
y <- T3
d <- Case
diff <- data.frame(x-y)
diff$colour[diff$x...y>=2]<-"red" #creating color vector
diff$colour[diff$x...y<= -2]<-"red"
diff$colour[diff$x...y<2 & diff$x...y>0] <-"blue"
diff$colour[diff$x...y> -2 & diff$x...y<0] <-"blue"
diff$colour[diff$x...y==0] <-"darkgreen"

dotchart(diff$x...y, groups=d,pch=25,ylim=c(-6,6),xlab="Difference AO (Before and After 1 Month)",
         color=diff$colour) #calling plot
abline(abline(v=0-c(-1,0,1),lty=5, bg=2)) #drawing abline

###T2xT3
x <- T2 #definig variables for the function
y <- T3
d <- Case
diff <- data.frame(x-y)
diff$colour[diff$x...y>=2]<-"red" #creating color vector
diff$colour[diff$x...y<= -2]<-"red"
diff$colour[diff$x...y<2 & diff$x...y>0] <-"blue"
diff$colour[diff$x...y> -2 & diff$x...y<0] <-"blue"
diff$colour[diff$x...y==0] <-"darkgreen"

dotchart(diff$x...y, groups=d,pch=25,ylim=c(-6,6),xlab="Difference AO (After AO Training and After 1 Month)",
         color=diff$colour) #calling plot
abline(abline(v=0-c(-1,0,1),lty=5, bg=2)) #drawing abline

#Creating data frames for agreement between times of application for PGY2 (T1, T2 and T3)
Inter1pg2<-data.frame(pg2$R1,pg2$R2) #Agreement between Before and After AO Training
agree(Inter1pg2, tolerance=0)
kappam.fleiss(Inter1pg2) #Kaplam-Fleiss agreement method for multiple raters

Inter2pg2<-data.frame(pg2$R1,pg2$R3) #Agreement between Before and After 1 month of AO training
agree(Inter2pg2, tolerance=0)
kappam.fleiss(Inter2pg2) #Kaplam-Fleiss agreement method for multiple raters

Inter3pg2<-data.frame(pg2$R2,pg2$R3) #Agreement between After and After 1 month of AO training
agree(Inter3pg2, tolerance=0)
kappam.fleiss(Inter3pg2) #Kaplam-Fleiss agreement method for multiple raters

#Creating data frames for agreement between times of application for PGY3 (T1, T2 and T3)
Inter1pg3<-data.frame(pg3$R1,pg3$R2) #Agreement between Before and After AO Training
agree(Inter1pg3, tolerance=0)
kappam.fleiss(Inter1pg3) #Kaplam-Fleiss agreement method for multiple raters

Inter2pg3<-data.frame(pg3$R1,pg3$R3) #Agreement between Before and After 1 month of AO training
agree(Inter2pg3, tolerance=0)
kappam.fleiss(Inter2pg3) #Kaplam-Fleiss agreement method for multiple raters

Inter3pg3<-data.frame(pg3$R2,pg3$R3) #Agreement between After and After 1 month of AO training
agree(Inter3pg3, tolerance=0)
kappam.fleiss(Inter3pg3) #Kaplam-Fleiss agreement method for multiple raters

#Graph 6 - Bland-Altman Variation
bland_altman_plot <- function(x,y,xlab="Average testresult", ylab="Deviation of experimental test")
{
  d <- y
  diff <- x - y
  
  plot(diff ~ d,pch=16,ylim=c(-6,6),xlab=xlab,ylab=ylab, axes=FALSE)
  abline(h=0-c(-1,0,1),lty=5, bg=2)
}

#####PGY2
bland_altman_plot(pg2$T3,pg2$T2,ylab="Difference AO Score",xlab="AO Responses")
axis(1,1:8,c("33A1","33A2","33A3","33B1","33B2","33B3","33C1","33C2"))
axis(2)
d1 <- pg2$T1
diff1 <- pg2$T1 -pg2$T2
points(diff1 ~ d1,pch=15,ylim=c(-6,6),xlab=xlab,ylab=ylab,col=90)
d2 <- pg2$T1
diff2 <- pg2$T1 - pg2$T3
points(diff2 ~ d2,pch=17,ylim=c(-6,6),xlab=xlab,ylab=ylab,col=35)
legend("topleft", "Time of Aplication", c("T1xT2","T1xT3","T2xT3"), horiz=TRUE,
       fill=c(90,35,"black"))

#####PGY3
bland_altman_plot(pg3$T3,pg3$T2,ylab="Difference AO Score",xlab="AO Responses")
axis(1,1:8,c("33A1","33A2","33A3","33B1","33B2","33B3","33C1","33C2"))
axis(2)
d1 <- pg3$T1
diff1 <- pg3$T1 -pg3$T2
points(diff1 ~ d1,pch=15,ylim=c(-6,6),xlab=xlab,ylab=ylab,col=90)
d2 <- pg3$T1
diff2 <- pg3$T1 - pg3$T3
points(diff2 ~ d2,pch=17,ylim=c(-6,6),xlab=xlab,ylab=ylab,col=35)
legend("topleft", "Time of Aplication", c("T1xT2","T1xT3","T2xT3"), horiz=TRUE,
       fill=c(90,35,"black"))
#######################################################################################
#AOAnalysisscript.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. You are free: to Share — to copy, distribute and transmit the work to Remix — to adapt the work, under the following conditions: Attribution — You must attribute the work in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work). Noncommercial — You may not use this work for commercial purposes. With the understanding that: Waiver — Any of the above conditions can be waived if you get permission from the copyright holder. Public Domain — Where the work or any of its elements is in the public domain under applicable law, that status is in no way affected by the license. Other Rights — In no way are any of the following rights affected by the license: Your fair dealing or fair use rights, or other applicable copyright exceptions and limitations; The author's moral rights; Rights other persons may have either in the work itself or in how the work is used, such as publicity or privacy rights. Notice — For any reuse or distribution, you must make clear to others the license terms of this work. The best way to do this is with a link to this web page. For more details see http://creativecommons.org/licenses/by-nc/3.0/
#######################################################################################
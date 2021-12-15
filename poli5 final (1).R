#Install all of the necessary packages and load them

install.packages('ggplot2')
install.packages('dplyr')
install.packages("stargazer")
install.packages('ggcorrplot')
install.packages('vtable')

library(vtable)
library(stargazer)
library(ggplot2)
library(dplyr)
library(ggcorrplot)

#Read in my data

data <- read.csv("C:/Users/Joshua Hussong/Downloads/nba_games.csv")

#Use stargazer to get a summary statistic output for my pdf

stargazer(data, type ='html',out='stargazer.html')


#Calculate median values for my summary statistic table
median(data$TeamPoints)
median(data$TotalRebounds)
median(data$Assists)
median(data$Steals)
median(data$Blocks)
median(data$Turnovers)
median(data$TotalFouls)
median(data$FieldGoals.)
median(data$FreeThrows.)



#Created an important variables value that would attempt to get the median quickly
important_variables <- c('TeamPoints','FieldGoals.','FreeThrows.','TotalRebounds','Assists','Steals','Blocks',
                         'Turnovers','TotalFouls')


#median(data[,important_variables])


#Exploratory Data Analysis to look at the max team points value for interest
data[,c('TeamPoints','FieldGoals.','FreeThrows.','TotalRebounds','Assits','Steals','Blocks',
                              'Turnovers','TotalFouls')]
max(data$TeamPoints)

#Violin Plot created in order to see the team points distribution

g <- ggplot(data, aes(Team, TeamPoints))
g + geom_violin() + 
  labs(title="Visual Plot", 
       subtitle="Team Points Distributed by Team",
       caption="Source: mpg",
       x="Team",
       y="TeamPoints")

#Scatterplot for Free Throw percentage and teampoints

s <- ggplot(data, aes(FreeThrows., TeamPoints))

s + geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="Comparing Free Throw Percentage with TeamPoints", 
       y="Team Points", 
       x="Free Throw Percentage", 
       title="Free Throw vs Team Points", 
       caption="Source: NBA Dataset 2014-2018")

#Scatterplot for Field Goal  percentage and teampoints


s1 <- ggplot(data, aes(FieldGoals., TeamPoints))

s1 + geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="Comparing Free Throw Percentage with TeamPoints", 
       y="Team Points", 
       x="Field Goal Percentage", 
       title="Field Goal vs Team Points", 
       caption="Source: NBA Dataset 2014-2018")

#Creating a correlational plot with ggcorrplot, the bubbles and values make it hard to read: gets put in Appendix
corr <- round(cor(data_subset),1)
nums <- unlist(lapply(data, is.numeric)) 
data_subset <- data[,nums]
data_subset <- subset(data_subset, select = -c('Opp.3PointShotsAttempted','Opp.FieldGoals') )

ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of NBA Data", 
           ggtheme=theme_bw)


#Creating a additional graph for win loss stacked bar histogram for Appendix
x1 <- ggplot(data, aes(Team))
x1 + geom_bar(aes(fill=WINorLOSS), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Win or Loss/Team in NBA", 
       subtitle="Higher Blue Value=more wins")



#Create a team points graph to look at team points histogram
TeamPoints_graph <- ggplot(data, aes(x=TeamPoints)) +
  geom_histogram(color = "black", fill = "white", binwidth=5) +
  geom_vline(aes(xintercept=mean(TeamPoints)),color="blue",
             linetype="dashed", size=1) +
  labs(title="Team Points Graph",x="Team Points", y = "Count")
TeamPoints_graph


#Not Useful because there are too many different values, rather
#use the summary statistic
table(data$TeamPoints)
summary(data$TeamPoints)
#Also Not Useful because there are too many different values,
#rather use the summary statistic
table(data$FieldGoals)
summary(data$FieldGoals)


#Create a FieldGoals Made Histogram to see visually how field goals made looks
FieldGoals_graph <- ggplot(data, aes(x=FieldGoals)) +
  geom_histogram(color = "black", fill = "white", binwidth=2) +
  geom_vline(aes(xintercept=mean(FieldGoals)),color="red",
             linetype="dashed", size=1) +
  labs(title="Field Goals Graph",x="Field Goals Made", y =
         "Count")
FieldGoals_graph

#Creating a graph that graphs field goal percentage and team points with colors based upon teams
x<- ggplot(data, aes(x=FieldGoals., y=TeamPoints, color=Team)) +
  labs(title="Field Goals Compared to Team
Points")+geom_point(size=1, shape=23)
x

#Creating a graph that graphs free throw percentage and team points with colors based upon teams

y<- ggplot(data, aes(x=FreeThrows., y=TeamPoints,  color=Team)) +
  labs(title="Free Throws Compared to Team
Points")+geom_point(size=1, shape=23)
y

#Create a correlational matrix between the variables of interest. 
sample_corr <- data[, c("TeamPoints", "FieldGoals.",'FreeThrows.')]
cor(sample_corr, method = "pearson", use = "complete.obs")



#Regression model with teampoints and free throw percentage
bi_reg <- lm(TeamPoints~FreeThrows.,data)
summary(bi_reg)

#Regression model with teampoints and field goal percentage
bi_reg2 <- lm(TeamPoints~FieldGoals.,data)
summary(bi_reg2)

#Regression model with teampoints and field goals in general to see the R-squared value
bi_reg3 <- lm(TeamPoints~FieldGoals,data)
summary(bi_reg3)

#Multiple Regressions model with teampoints and important values of interest within the dataset
multi_reg <- lm(TeamPoints~FieldGoals.+FreeThrows.+TotalRebounds+Assists+Steals+Blocks+Turnovers,data=data)
summary(multi_reg)


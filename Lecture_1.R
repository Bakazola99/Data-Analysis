student <- read.csv('student.csv', header = T)

stud <- student[,-c(1:2)]
head(stud)

library(ggplot2)
library(tidyverse)
library(ggpubr)

plot(stud[,-1])

d<-density(stud$MATH)
plot(d, main = "Density plot of Math scores")

d2<-density(stud$ENGLISH)
plot(d2, main = "Density plot of English scores")

d3<-density(stud$INTELIGENCE)
plot(d3, main = "Density plot of Intelligence scores")

######### Data Wrangling ##########
stud_1 <- stud %>% 
  gather(Subjects, Scores, 2:7) %>% 
  group_by(Subjects,GENDER) %>% 
  summarise(avg_scores = mean(Scores), sd_scores = sd(Scores))
stud_1

######## Visualisation of the Students subject scores######
ggplot(stud_1, aes(x= reorder(Subjects,avg_scores), 
                   y= avg_scores, 
                   fill= reorder(GENDER, avg_scores)))+
  geom_bar(position = "dodge", stat = "identity" )+
  geom_errorbar(aes(ymin= avg_scores-sd_scores,
                    ymax = avg_scores+sd_scores),
                width= 0.2, 
                position = position_dodge(width = 0.9))+
  labs(title = "Scores of Students To Determine IQ",
       subtitle = "IQ Determination",
       caption = "~Baki, Azeez Olalekan",
       fill = "Gender",
       y= "Average scores",
       x = "Subjects")+
  theme_classic()+
  scale_fill_brewer()

####### Determining the correlation #########
subject_cor <- cor(stud[,-1], method = 'pearson')
subject_cor

library(corrplot)
sub_cor_plot <- corrplot(subject_cor, method = 'number', type = "lower")

######## Showing line graph #######
sub_line <- ggplot(stud, aes(x= ECONOMICS, y= INTELIGENCE))+
  geom_point()+
  geom_smooth( method = "lm", se = 0, formula = y~x-1)+
  stat_cor(label.x = 40, label.y = 9)+
  stat_regline_equation()+
  theme_classic()+
  labs(title = "INTELLIGENCE BY ECONOMICS SCORES")
sub_line

######## Scores Difference, Using T-test #########
scores_diff <- t.test(stud$MATH, stud$ENGLISH)
scores_diff

subject_diff <- t.test(stud$ECONOMICS)
subject_diff

####### Using ANOVA for Difference among means #####
subjects_mean_diff <- aov(avg_scores~., 
                          data=stud_1)
summary(subjects_mean_diff)

TukeyHSD(aov(avg_scores~., 
             data=stud_1))

library(gplots)
plotmeans(INTELIGENCE~GENDER, 
          data=stud, main= "Means Difference",
          ylab = 'Intelligence',
          barcol = "black",
          col = "red")

######### Building model for prediction #######
model <- lm(INTELIGENCE~MATH+ENGLISH+BIOLOGY+ECONOMICS+AGRIC, 
            data=stud)
model
summary(model)

######## Predicting new students IQ ######
new_stu <- data.frame("GENDER"=c("MALE",'FEMALE', "MALE", "FEMALE"), 
                      "MATH"=c(89,90,80, 97), 
                      'ENGLISH'=c(90,87.99, 99.99, 97), 
                      'BIOLOGY'=c(90.5,80, 89,90.4),
                      'ECONOMICS'= c(97,97,90,89), 
                      'AGRIC'= c(90.5,80.5,85.5,97.4))
new_stu_IQ <- predict(model, new_stu)
new_stu_IQ

new_stu$INTELLIGENCE <- new_stu_IQ
View(new_stu)

new_stu_IQ_scores <- write.csv(new_stu, "new_students_IQ_score.csv")

library(psych)
describeBy(stud, group = stud$GENDER)

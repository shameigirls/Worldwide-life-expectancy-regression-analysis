
set.seed(597257)
library(ggplot2)
library(broom)
library('visdat')



# Read in the heart dataset under same dictionary
life <- read.csv('Life Expectancy Data.csv')
head(life)
# dimension of dataset
dim(life) 

# 2) check for missing values:
table(is.na(life))

#check number/percentage of missing value (0 for other colums)
table(is.na(life$percentage.expenditure))
sum(is.na(life$percentage.expenditure))/dim(life)[1]

table(is.na(life$GDP_per_capita))
sum(is.na(life$GDP_per_capita))/dim(life)[1]

#delete/fill NA
life=subset(life, select = -c(percentage.expenditure))
life$GDP_per_capita[which(is.na(life$GDP_per_capita))]=c(28401.7,513.314364,1373.814438,817.1226341,1151.421565,1505.810949)

# 3) EDA of y:
summary(life$Life.expectancy)
# histogram of charges
hist(life$Life.expectancy,main='Histogram of life expectancy',
     xlab="life expectancy")
# boxplot of charges
boxplot(life$Life.expectancy) 
title(main='Boxplot of life expectancy',
      ylab="life expectancy")

# log (log-e/ln) transformations:
# histogram of log charges
hist(log(life$Life.expectancy), xlab = "log life expectancy", main="Histogram of log life expectancy")
# boxplot of log charges
boxplot(log(life$Life.expectancy)) 
title(main='Boxplot of log life expectancy',
      ylab="log life expectancy")

# 4) EDA of Polio:
summary(life$Polio)
# histogram 
hist(life$Polio,main='histogram of Life expectancy')
# boxplot 
boxplot(life$Polio) 
title(main='Boxplot of Life expectancy')

# 4) EDA of Government.expenditure:
summary(life$Government.expenditure)
# histogram of charges
hist(life$Government.expenditure,main='histogram of Life expectancy')
# boxplot of charges
boxplot(life$Government.expenditure) 
title(main='Boxplot of Life expectancy')


# 5) EDA of $GDP_per_capita:
summary(life$GDP_per_capita)
hist(life$GDP_per_capita,main='histogram of Life expectancy')
boxplot(life$GDP_per_capita) 
title(main='Boxplot of Life expectancy')


# 5) EDA of $Schooling:
summary(life$Schooling)
hist(life$Schooling,main='histogram of ')
boxplot(life$Schooling) 
title(main='Boxplot of ')

#GDP per capita & Schooling are cbout economic and society，may have correlation to status--i.e. be confounder,so we want to check that

ggplot(data=life, aes(x=Schooling,y=Life.expectancy,colour = Status)) +
  geom_point(size=1.2)+
  labs(title=" ")+
  theme_bw()+
  theme(panel.grid.major =element_blank(),plot.title = element_text(hjust = 0.5))

ggplot(data=life, aes(x=GDP_per_capita,y=Life.expectancy,colour = Status)) +
  geom_point(size=1.2)+
  labs(title=" ")+
  theme_bw()+
  theme(panel.grid.major =element_blank(),plot.title = element_text(hjust = 0.5))


#hypothesis
test_age=t.test(life$Life.expectancy[life$Status=='Developing'],life$Life.expectancy[life$Status=='Developed'])
test_age
tidy(test_age)

#simple
slr=lm(Life.expectancy~Polio, data=life)
summary(slr)
tidy(slr)
# ggplot(data=life, aes(x=Polio,y=Life.expectancy)) + 
#   geom_point(size=1.2)+
#   geom_smooth(method = "lm", se = FALSE, color = "orange", size=1.4)+
#   # annotate("text", x=27, y=9, label="log-charges=7.74+0.035*age",size=4)+ 
#   labs(title=" ")+
#   theme_bw()+
#   theme(panel.grid.major =element_blank(),plot.title = element_text(hjust = 0.5))

# multivariable
#least 2 slides
mlr=lm(Life.expectancy~Polio+Government.expenditure+GDP_per_capita+Schooling+Status, data=life)
summary(mlr)
tidy(mlr)

# cor(subset(life, select = -c(Country,Status)))


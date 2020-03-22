setwd("~/ds202_lab4")


#packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

#read in data
defense <- read_excel('cyclonesFootball2019.xlsx', sheet='Defensive')
offense <- read_excel('cyclonesFootball2019.xlsx', sheet='Offensive')
bio <- read_excel('cyclonesFootball2019.xlsx', sheet='Biography')
str(defense)
str(offense)
str(bio)



#Clean Data Q1
offense$Name = as.factor(offense$Name)
offense$Opponent_Opponent = as.factor(offense$Opponent_Opponent)
defense$Name = as.factor(defense$Name)
defense$Opponent_Opponent = as.factor(defense$Opponent_Opponent)
bio$Name = as.factor(bio$Name)


#Clean Data Q2
defClean = defense %>% mutate_if(is.character, as.numeric)
offClean = offense %>% mutate_if(is.character, as.numeric)
bioClean = bio %>% mutate_at("Weight", as.numeric)
str(offClean)


#Clean Data Q3
bioClean = bioClean %>%
  separate('Height', c("Feet", "Inches"), 
           sep = "-(?=[^ ]+$)", remove = FALSE) %>%
  mutate_at("Feet", as.numeric) %>%
  mutate_at("Inches", as.numeric)
bioClean$Height = bioClean$Feet * 12 + bioClean$Inches
bioClean = bioClean %>% select(-Feet, -Inches)


#CD P2 Q1
#?pivot_longer
defCleanP2 = pivot_longer(defClean, cols = Tackles_Solo:Pass_PB, names_to = "Statistic", values_to = "Count", values_drop_na = TRUE)
head(defClean2,5)


#CD P2 Q2
ggplot(data=defCleanP2, aes(defCleanP2$Count)) + geom_histogram(binwidth = 1, color="black", fill="lightblue") +
  facet_wrap(~ defCleanP2$Statistic) + labs(title=" Defense Skills Plots",y="Frequency", x="Number of Occurrences") 

  
#CD P2 Q3     

defCleanP2Q3 = defCleanP2 %>%
  filter(Statistic == "Tackles_Solo", Opponent_Opponent %in% c("West Virginia","Kansas State")) %>% 
  select(-Statistic) %>%
  pivot_wider(names_from = Opponent_Opponent, values_from = Count)
ggplot(defCleanP2Q3, aes(x=defCleanP2Q3$`West Virginia`, y=defCleanP2Q3$`Kansas State`, color=green)) +
  geom_point(size=3, shape=20,fill="lightblue", color="black") + labs(title="Solo Tackles in WVU and KSU Games", x="West Virgina Solo Tackles", y="Kansas State Solo Tackles") 










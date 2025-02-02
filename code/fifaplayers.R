rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

setwd("C:\\Users\\Olga\\0. VS Code\\Github\\visualizations\\")

df<-read.csv("data\\fifa_players.csv")

glimpse(df)

#Data Manipulation
df_clean<- df %>%  mutate(
  Wage = str_remove_all(Wage, "€"),  # Remove euro symbol (€)
  Wage = str_replace(Wage, "K", ""),  
  "Wage_K" = as.numeric(Wage),
  
  Finishing = as.numeric(str_remove(Finishing, "\\+\\d+")),  # Remove FIFA skill growth indicators (e.g., "72+2")
  
  Role = case_when(
    str_detect(Best.Position, "ST|CF|LW|RW") ~ "Attacker",
    str_detect(Best.Position, "CM|CAM|CDM|LM|RM|LWB|RWB") ~ "Midfielder",
    str_detect(Best.Position, "CB|LB|RB") ~ "Defender",
    str_detect(Best.Position, "GK") ~ "Goalkeeper",
    TRUE ~ "Unknown"  # Default if no match
  ),
  Performance_Score= case_when(
    str_detect(Role,  "Attacker") ~ (5 * as.numeric(Finishing)) +
      (4 * as.numeric(Dribbling)) +
      (3 * as.numeric(Shot.Power)) +
      (2 * as.numeric(Sprint.Speed))+
      (1 * as.numeric(Attack.Position)),
    str_detect(Role, "Midfielder") ~ (5 * as.numeric(Short.Passing)) +
      (3 * as.numeric(Dribbling)) +
      (4 * as.numeric(Vision)) +
      (2 * as.numeric(Long.Passing)) +
      (1 * as.numeric(Stamina)),
    str_detect(Role, "Defender") ~ (5 * as.numeric(Standing.Tackle)) +
      (4 * as.numeric(Interceptions)) +
      (3 * as.numeric(Strength)) +
      (2 * as.numeric(Defensive.Awareness)) +
      (1 * as.numeric(Heading.Accuracy)),
    str_detect(Role, "Goalkeeper") ~ (5 * as.numeric(GK.Reflexes)) +
      (4 * as.numeric(GK.Diving)) +
      (3 * as.numeric(GK.Handling)) +
      (2 * as.numeric(GK.Positioning))+
      (1 * as.numeric(GK.Positioning)),
    TRUE ~ as.numeric(Overall.Score)  # Default if no match
  )) %>%
  filter(Role != "Unknown", !is.na(Wage_K), Wage_K != "")


ggplot(data=df_clean, aes(x=Wage_K, y=Performance_Score))+
  geom_point()+
  facet_wrap(~League)


england<-df_clean%>% filter(League== 	"Premier League (England)")

ggplot(data=england, aes(x=Wage_K, y=Performance_Score))+
  geom_point()+
  facet_wrap(~Role)


ggplot(data=england, aes(x=Wage_K, y=Overall.Score))+
  geom_point()+
  facet_wrap(~Role)+
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  scale_x_continuous(labels = scales::comma, name = "Wage (in K)") +
  theme_minimal()


df_clean%>% select(Position, Role, Best.Position)

df%>% distinct(Best.Position)

GK <- df %>% filter(Position == "GK, GK")

df%>% distinct(League)



# 1️⃣	Premier League	England
# 2️⃣	La Liga	Spain
# 3️⃣	Bundesliga	Germany
# 4️⃣	Serie A	Italy
# 5️⃣	Ligue 1	France
# 6️⃣	Eredivisie	Netherlands
# 7️⃣	Primeira Liga	Portugal
# 8️⃣	Major League Soccer (MLS)	USA
# 9️⃣	Pro League (Saudi Arabia)	Saudi Arabia
# 🔟	Süper Lig	Türkiye

# Erasmus Mobility: Who Goes Where

## Load library
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(tibble)
install.packages("circlize")  

library(circlize)

install.packages("ggplotify")   
library(ggplotify)
install.packages("cowplot")

library(cowplot)
## Import data
setwd("C:\\Users\\Olga\\0. VS Code\\Github\\visualizations\\")
df <- read_excel("data\\Erasmus-KA2-Mobility-Data.xlsx", sheet = "E+ KA2 Data (Since 2014)")

df <- df %>% 
  select(`Sending Country`, `Receiving Country`, `Actual Participants (Contracted Projects)`)%>% 
  separate(
    `Sending Country`,
    into = c("sending_country_code","sending_country_name"),
    sep  = " - ",
    extra = "merge", fill = "right",
    remove = TRUE
  ) %>% 
  separate(
    `Receiving Country`,
    into = c("receiving_country_code","receiving_country_name"),
    sep  = " - ",
    extra = "merge", fill = "right",
    remove = TRUE
  )

# Get top 10 sending countries
top_10_sending<-df%>%
  filter(sending_country_code!=receiving_country_code)%>%
  group_by(sending_country_name)%>%
  summarise(amount=sum(`Actual Participants (Contracted Projects)`))%>%
  arrange(desc(amount))%>%
  head(10)

top_10_receiving<-df%>%
  filter(sending_country_code!=receiving_country_code)%>%
  group_by(receiving_country_name)%>%
  summarise(amount=sum(`Actual Participants (Contracted Projects)`))%>%
  arrange(desc(amount))%>%
  head(10)

top_countries<-unique(c(top_10_sending$sending_country_name, top_10_receiving$receiving_country_name))
top_countries

df_short<-df%>%
  filter(sending_country_name %in% top_countries,
         receiving_country_name %in% top_countries,
         sending_country_name != receiving_country_name) %>%
  group_by(sending_country_name,receiving_country_name)%>%
  summarise(amount = sum(`Actual Participants (Contracted Projects)`), .groups="drop") %>%
  arrange(desc(amount))

df_mat <- df %>%
  filter(sending_country_name %in% top_countries,
         receiving_country_name %in% top_countries,
         sending_country_name != receiving_country_name) %>%
  group_by(sending_country_name, receiving_country_name) %>%
  summarise(amount = sum(`Actual Participants (Contracted Projects)`), .groups="drop") %>%
  pivot_wider(names_from = receiving_country_name,
              values_from = amount,
              values_fill = 0) %>%
  column_to_rownames("sending_country_name") %>%
  as.matrix()

pal <- c(
  "#002765","#0061fd","#1cc6ff","#00b661","#5bf34d",
  "#ffdd00","#ff7d00","#da2818","#ff006d","#8f00ff"
)

chordDiagram(df_short, grid.col = pal)

p<-recordPlot()
as.ggplot(ggdraw(p))+
  labs(title="ERASMUS STUDENT MOBILITY",
       subtitle="Movement of Erasmus participants among the leading countries (2014–2023)",
       caption="Data from erasmus-plus | Created by Litvinova Olga")+
  theme(text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, face="bold", size=20),
        plot.subtitle=element_text(hjust=0.5, size=12, margin=margin(t=10)),
        plot.caption=element_text(size=10, hjust=0.95, margin=margin(b=12)),
        plot.margin   =margin(t=20))

p




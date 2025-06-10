
# Erasmus Mobility: Who Goes Where

## Load library
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
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

df<-df%>%
  filter(sending_country_name %in% top_countries)%>%
  filter(receiving_country_name %in% top_countries)%>%
  group_by(sending_country_name,receiving_country_name)%>%
  summarise(amount=sum(`Actual Participants (Contracted Projects)`))%>%
  arrange(desc(amount))
  

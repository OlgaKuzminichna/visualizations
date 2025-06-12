
# Erasmus Mobility: Who Goes Where

## Load library
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(tibble)
library(circlize)
library(ggplotify)
library(cowplot)
library(gridGraphics)

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
# Get top 10 receiving countries
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
  summarise(amount = sum(`Actual Participants (Contracted Projects)`)/1000, .groups="drop") %>%
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
  "#1f77b4",  # muted blue
  "#ff7f0e",  # safety orange
  "#2ca02c",  # cooked asparagus green
  "#d62728",  # brick red
  "#9467bd",  # muted purple
  "#8c564b",  # chestnut brown
  "#e377c2",  # raspberry yogurt pink
  "#7f7f7f",  # mid gray
  "#bcbd22",  # curry yellow-green
  "#17becf"   # blue-teal
)

 chordDiagram(df_short, grid.col = pal)
 p<-recordPlot()
 p<-as.ggplot(ggdraw(p))+
  labs(title="Erasmus Mobility: Who Goes Where",
       subtitle="Movement of Erasmus participants(in thousands) among the leading countries (2014–2023)",
       caption="Data from erasmus-plus | Created by Litvinova Olga")+
   theme(
     plot.title       = element_text(
       hjust = 0.5,
       face  = "bold",
       size  = 20,
       color = "#E2657A"
     ),
     plot.subtitle    = element_text(
       hjust = 0.5,
       size  = 12,
       margin = margin(t = 10)
     ),
     plot.caption     = element_text(
       size = 10,
       hjust = 0.95,
       margin = margin(b = 12)
     ),
     plot.margin      = margin(20, 20, 20, 20)
   )

print(p)

ggsave("erasmus.jpeg", height=9, width=9)
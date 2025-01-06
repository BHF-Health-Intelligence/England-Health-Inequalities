##### England's Health Inequalities Analysis script #####
### Authors: Camila Pulliza
### Date created: 06/01/2025

##### Packages #####
options(scipen = 999)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(openxlsx)
library(magrittr)
library(stringr)
library(curl)
library(ggrepel)
library(readxl)
library(RColorBrewer)
library(sf)
library(qicharts2)
library(showtext)
library(scales)
library(patchwork)
library(chron)
library(readODS)
library(lubridate)
library(sf)
library(geojsonsf)
library(gginnards)
library(rio)
library(janitor)
library(data.table)
library(formattable)
library(flextable)
library(readr)

######### Section 1 - RUN BHF STYLE FUNCTIONS########

##ADD FONTS##
#Beats  Will need to update local file location for .otf font files

font_add("bhf_beats_bold", "C:/Users/almondth/OneDrive - British Heart Foundation/Documents/R/Fonts/Beats/OTF/BHFBeats-Bold.otf")
font_add("bhf_beats_light", "C:/Users/almondth/OneDrive - British Heart Foundation/Documents/R/Fonts/Beats/OTF/BHFBeats-Light.otf")
font_add("bhf_beats_reg", "C:/Users/almondth/OneDrive - British Heart Foundation/Documents/R/Fonts/Beats/OTF/BHFBeats-Regular.otf")
font_add("bhf_ginger_bold", "C:/Users/almondth/OneDrive - British Heart Foundation/Documents/R/Fonts/Ginger/OTF/F37Ginger-Bold.otf")
font_add("bhf_ginger_light", "C:/Users/almondth/OneDrive - British Heart Foundation/Documents/R/Fonts/Ginger/OTF/F37Ginger-Light.otf")
font_add("bhf_ginger_reg", "C:/Users/almondth/OneDrive - British Heart Foundation/Documents/R/Fonts/Ginger/OTF/F37Ginger-Regular.otf")
showtext_auto()

t_font <- list(family = "bhf_ginger_reg", size = 14)

#If you are using showtext in RMarkdown documents you don’t have to use showtext_auto(). 
#That will set up the wrong dpi and the text will look too small. 
#You need to add fig.showtext=TRUE to the chunk settings.

###SET COLOUR PALETTES##

bhf_colours <- c(
  `Bright Red` = "#FF0030",
  `Dark Red` = "#8C0032",
  `Medium Red` = "#D20019",
  `Rubine Red` = "#E71348",
  `Light Blue` = "#2D91FF",
  `Indigo` = "#500AB4",
  `Pinkish` = "#FF3C64",
  `Orange` = "#FF873C",
  `Yellow` = "#FFBE32",
  `Light green` = "#19D79B",
  `Dark green` = "#00A06E",
  `Dark grey` = "#474E5A",
  `White` = "#FFFFFF"
)

bhf_cols <- function(...) {
  cols <- c(...)
  if(is.null(cols))
    return(bhf_colours)
  bhf_colours[cols]
}

#Define palettes

bhf_palettes <- list(
  `reds` = bhf_cols("Bright Red","Dark Red","Rubine Red", "Medium Red"),
  `not reds` = bhf_cols("Bright Red","Light Blue","Indigo"),
  `gradient_1` = bhf_cols("Dark Red","Medium Red"),
  `gradient_2` = bhf_cols("Medium Red","Bright Red"),
  `gradient_3` = bhf_cols("Bright Red","Rubine Red"),
  `gradient_4` = bhf_cols("Bright Red","White"),
  `secondaries` = bhf_cols("Light Blue", "Indigo","Pinkish",
                           "Orange","Yellow","Light green",
                           "Dark green","Dark grey"),
  `expanded secondaries` = bhf_cols("Bright Red", "Light Blue", "Indigo","Pinkish",
                                    "Orange","Yellow","Light green",
                                    "Dark green","Dark grey"),
  `red and light blue` = bhf_cols("Bright Red", "Light Blue"),
  `red, blue, indigo` = bhf_cols("Bright Red", "Light Blue", "Indigo"),
  `red, blue, indigo, orange` = bhf_cols("Bright Red", "Light Blue", "Indigo", "Orange"),
  `red to yellow` = bhf_cols("Bright Red", "Indigo", "Light Blue", "Light green", "Yellow")
  
)

bhf_pal<- function(palette = "reds", reverse = FALSE, ...) {
  pal <- bhf_palettes[[palette]]
  if(reverse) pal <- rev(pal)
  colorRampPalette(pal,...)
}

#Create scale_colour and scale_fill functions

scale_color_bhf <- function(palette = "reds", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bhf_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_color_bhf_cont <- function(palette = "reds", discrete = FALSE, reverse = TRUE, ...) {
  pal <- bhf_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_bhf <- function(palette = "reds", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bhf_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


scale_fill_bhf_cont <- function(palette = "reds", discrete = FALSE, reverse = TRUE, ...) {
  pal <- bhf_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

##BUILD FORMATTING FUNCTION##


#BHF everything 

bhf_style <- function (bhf_brand,textsize=10) 
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold", 
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg", 
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),  
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 #legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                     color = "#191919"),  
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"))#,
  #axis.line = ggplot2::element_blank(), 
  #panel.grid.minor = ggplot2::element_blank(), 
  # panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"), 
  #panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
  #strip.background = ggplot2::element_rect(fill = "white"), 
  #strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}

bhf_map_style <- function (bhf_brand,textsize=10) 
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold", 
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg", 
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),  
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 #legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                     color = "#191919"),  
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.line = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 # panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white")) 
  #strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}

####### Section 2 - Set working directory ######

setwd('C:/Users/pullizac/OneDrive - British Heart Foundation/Documents/Projects/Health Inequalities/England HI page/R datasets')
source("C:/Users/pullizac/OneDrive - British Heart Foundation/Documents/R/BHF theme 1.R")

###### Section 3 - Analysis #######

#READ ALL DATASETS
{
#Read the conditions dataset
prevalence_conditions <- read.xlsx("oct_update_conditions.xlsx")
hf_cvd_prevent <- read.xlsx("oct_update_cvd_prevent_hf.xlsx")
cvd_cvd_prevent <- read.xlsx("oct_update_cvd_prevent_cvd.xlsx")
af_cvd_prevent <- read.xlsx("oct_update_cvd_prevent_af.xlsx")


#Read the risk factor dataset
risk_factors <- read.xlsx("august_update_risk_factors.xlsx")
hyp_cvd_prevent <- read.xlsx("oct_update_cvd_prevent_hyp.xlsx")
cholesterol <- read.xlsx("cholesterol_hse_oct.xlsx")


#Read the age-standardised mortality rates (ASDRs) datasets
asdr_data <- read.csv("asdr_combined_imd_nov.csv")

#Read the life expectancy dataset
life_expectancy <- read.xlsx("life_expectancy_data.xlsx")

#Read the drug dataset
drugs <- read.csv("new_drugs.csv")

#Read the cardiac rehab audit dataset
cardiac_rehab <- read.xlsx("cardiac_rehab_sept.xlsx")


#Read the admissions dataset
admissions_data <- read.xlsx("august_update_hospital_admissions.xlsx")


#Read the GP survey dataset
survey_2 <- read.xlsx("survey_2.xlsx")

}

#SETTING IMD AS A FACTOR#
{
#Order for the conditions dataset by deprivation
prevalence_conditions$Value <-as.numeric(prevalence_conditions$Value)
prevalence_conditions$Category <- as.factor(prevalence_conditions$Category)

prevalence_conditions$Category <- gsub("1 - Most deprived decile", "1", prevalence_conditions$Category)
prevalence_conditions$Category <- gsub("10 - Least deprived decile ", "10", prevalence_conditions$Category)

prevalence_conditions$Category <- factor(prevalence_conditions$Category, levels = c("1",
                                                                      "2",
                                                                      "3",
                                                                      "4",
                                                                      "5",
                                                                      "6",
                                                                      "7",
                                                                      "8",
                                                                      "9",
                                                                      "10"))
#Order for the heart failure dataset by deprivation  
hf_cvd_prevent$Value <-as.numeric(hf_cvd_prevent$Value)
hf_cvd_prevent$IMD_category <- as.factor(hf_cvd_prevent$IMD_category)

hf_cvd_prevent$IMD_category <- gsub("1 - most deprived","1",hf_cvd_prevent$IMD_category)
hf_cvd_prevent$IMD_category <- gsub("5 - least deprived","5",hf_cvd_prevent$IMD_category)

hf_cvd_prevent$IMD_category <- factor(hf_cvd_prevent$IMD_category, levels = c("1",
                                                                                    "2",
                                                                                    "3",
                                                                                    "4",
                                                                                    "5")) 

#Order for the CVD dataset by deprivation
cvd_cvd_prevent$IMD_category <- as.factor(cvd_cvd_prevent$IMD_category)

cvd_cvd_prevent$IMD_category <- gsub("1 - most deprived","1",cvd_cvd_prevent$IMD_category)
cvd_cvd_prevent$IMD_category <- gsub("5 - least deprived","5",cvd_cvd_prevent$IMD_category)

cvd_cvd_prevent$IMD_category <- factor(cvd_cvd_prevent$IMD_category, levels = c("1",
                                                                              "2",
                                                                              "3",
                                                                              "4",
                                                                              "5")) 
#Order for the atrial fibrillation dataset by deprivation
af_cvd_prevent$Value <-as.numeric(af_cvd_prevent$Value)
af_cvd_prevent$IMD_category <- as.factor(af_cvd_prevent$IMD_category)

af_cvd_prevent$IMD_category <- gsub("1 - most deprived","1",af_cvd_prevent$IMD_category)
af_cvd_prevent$IMD_category <- gsub("5 - least deprived","5",af_cvd_prevent$IMD_category)

af_cvd_prevent$IMD_category <- factor(af_cvd_prevent$IMD_category, levels = c("1",
                                                                                "2",
                                                                                "3",
                                                                                "4",
                                                                                "5")) 
#Order for the risk factors dataset by deprivation
risk_factors$Value <-as.numeric(risk_factors$Value)
risk_factors$Category <- as.factor(risk_factors$Category)

risk_factors$Category <- gsub("1 - Most deprived decile", "1", risk_factors$Category)
risk_factors$Category <- gsub("10 - Least deprived decile", "10", risk_factors$Category)

risk_factors$Category <- factor(risk_factors$Category, levels = c("1",
                                                                                    "2",
                                                                                    "3",
                                                                                    "4",
                                                                                    "5",
                                                                                    "6",
                                                                                    "7",
                                                                                    "8",
                                                                                    "9",
                                                                                    "10"))
#Order for the hypertension dataset by deprivation
hyp_cvd_prevent$Value <-as.numeric(hyp_cvd_prevent$Value)
hyp_cvd_prevent$IMD_category <- as.factor(hyp_cvd_prevent$IMD_category)

hyp_cvd_prevent$IMD_category <- gsub("1 - most deprived","1",hyp_cvd_prevent$IMD_category)
hyp_cvd_prevent$IMD_category <- gsub("5 - least deprived","5",hyp_cvd_prevent$IMD_category)

hyp_cvd_prevent$IMD_category <- factor(hyp_cvd_prevent$IMD_category, levels = c("1",
                                                                              "2",
                                                                              "3",
                                                                              "4",
                                                                              "5"))  

#Order for the cholesterol dataset by deprivation
cholesterol$Category <- factor(cholesterol$Category, levels = c("1",
                                                                                "2",
                                                                                "3",
                                                                                "4",
                                                                                "5")) 

#Order for the ASDR dataset by deprivation
asdr_data$Value <-as.numeric(asdr_data$Value)
asdr_data$Category <- as.factor(asdr_data$Category)
asdr_data$Category <- gsub("1 - Most deprived decile","1",asdr_data$Category)
asdr_data$Category <- gsub("10 - Least deprived decile","10",asdr_data$Category)
                                     
asdr_data$Category <- factor(asdr_data$Category, levels = c("1",
                                                            "2",
                                                            "3",
                                                            "4",
                                                            "5",
                                                            "6",
                                                            "7",
                                                            "8",
                                                            "9",
                                                            "10"))

#Order for the prescriptions dataset by deprivation
drugs$rate <- as.numeric(drugs$rate)
drugs$decile <- as.factor(drugs$decile)
drugs$decile<- factor(drugs$decile, levels = c("1",
                                                                  "2",
                                                                  "3",
                                                                  "4",
                                                                  "5",
                                                                  "6",
                                                                  "7",
                                                                  "8",
                                                                  "9",
                                                                  "10"))



#Order for the cardiac rehab dataset by deprivation
cardiac_rehab_imd <- cardiac_rehab %>%
  filter (IMD_Quintile %in% c(1:5))

cardiac_rehab_imd$percentage_uptake <-as.numeric(cardiac_rehab_imd$percentage_uptake)
cardiac_rehab_imd$IMD_Quintile <- as.factor(cardiac_rehab_imd$IMD_Quintile)

cardiac_rehab_imd$IMD_Quintile <- factor(cardiac_rehab_imd$IMD_Quintile, levels = c("1",
                                                                                "2",
                                                                                "3",
                                                                                "4",
                                                                                "5"))  



#Order for the admissions dataset by deprivation
admissions_data$Category <- as.factor(admissions_data$Category)
admissions_data$Category <- gsub("1 - Most deprived decile ", "1", admissions_data$Category)
admissions_data$Category <- gsub("10 - Least deprived decile ", "10", admissions_data$Category)

admissions_data$Category <- factor(admissions_data$Category, levels = c("1",
                                                    "2",
                                                    "3",
                                                    "4",
                                                    "5",
                                                    "6",
                                                    "7",
                                                    "8",
                                                    "9",
                                                    "10"))

#Order for the GP survey dataset by deprivation
survey_2$Value <-as.numeric(survey_2$Value)
survey_2$Category <- as.factor(survey_2$Category)

survey_2$Category <- gsub("Deprivation quintile 1","1",survey_2$Category)
survey_2$Category <- gsub("Deprivation quintile 5","5",survey_2$Category)

survey_2$Category <- factor(survey_2$Category, levels = c("1",
                                                              "2",
                                                              "3",
                                                              "4",
                                                              "5"))
}

#RISK FACTORS#
{
#Plot obesity prevalence (%) by deprivation
obesity <- risk_factors %>%
  filter(Indicator.Name %in% "Overweight (including obesity) prevalence in adults (18+ yrs)" |
           Indicator.Name %in% "Obesity prevalence in adults")
  
obesity_labels <- as_labeller(c("Overweight (including obesity) prevalence in adults (18+ yrs)"=
                                "Overweight (including obesity)",
                                "Obesity prevalence in adults"="Obesity"))
  
obesity_plot <- ggplot(obesity, aes(x=Time.period, y=Value,colour = Category, group = Category)) +
  facet_wrap(~Indicator.Name, labeller = obesity_labels)+
  geom_point(size = 2.5, alpha = 0.7) +
  geom_line(size=1.5, alpha = 0.5)+
  ylab("Prevalence")+
  labs(color= str_wrap("Index of Multiple Deprivation Decile",15),
       title = str_wrap("Percentage of adults with a BMI classified as overweight or obese, 2015/16 to 2022/23", 80),
       subtitle = str_wrap("1 = most deprived decile, 10 = least deprived decile"),
       caption = "Source: Office for Health Improvement and Disparities (OHID) Fingertips") +
  bhf_style(textsize = 16) +
  scale_color_bhf("red and light blue")+
  scale_y_continuous(limits = c(0.15,.8),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35)
        ,axis.title.x=element_blank(),
        legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        plot.caption=element_text(size = 10),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        strip.text = element_text(size = 16))
obesity_plot


#Plot physical inactivity prevalence (%) by deprivation
physical_activity <- risk_factors %>%
  filter(Indicator.Name %in% "Percentage of physically inactive adults"| 
           Indicator.Name %in% "Percentage of physically active adults")

physical_activity$Indicator.Name <- gsub("Percentage of physically inactive adults",
                                         "Physically inactive (<30 mins weekly)",physical_activity$Indicator.Name)
physical_activity$Indicator.Name <- gsub("Percentage of physically active adults",
                                         "Meets physical activity guidelines (150+ mins a week)",
                                         physical_activity$Indicator.Name)

pa_plot <- ggplot(physical_activity, aes(x=Time.period, y=Value,colour = Category, group = Category)) +
  facet_wrap(~Indicator.Name,labeller = label_wrap_gen())+
  geom_point(size = 2.5, alpha = 0.7) +
  geom_line(size=1.5, alpha = 0.5)+
  ylab("Percentage")+
  labs(color= str_wrap("Index of Multiple Deprivation Decile",15),
       title = str_wrap("Physical activity levels of adults (19+) in England by deprivation decile, 2015/16 to 2022/23", 100),
       subtitle = str_wrap("1 = most deprived decile, 10 = least deprived decile"),
       caption = "Source: Sport England (SE) via Office for Health Improvement and Disparities (OHID) Fingertips") +
  bhf_style(textsize = 16) +
  scale_color_bhf("red and light blue")+
  scale_y_continuous(limits = c(0.1,.8),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35)
        ,axis.title.x=element_blank(),
        legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        strip.text = element_text(size = 14),
        plot.caption=element_text(size = 10),
        plot.caption.position = "plot")
pa_plot


#Plot smoking prevalence (%) by deprivation
smoking <- risk_factors %>%
  filter(Indicator.Name %in% "Smoking prevalence in adults")

smoking_plot <- ggplot(smoking, aes(x=Time.period, y=Value,colour = Category, group = Category)) +
  geom_point(size = 4, alpha = 1) +
  geom_line(size=2, alpha = 1)+
  ylab("Percentage")+
  labs(color= str_wrap("Index of Multiple Deprivation Decile",15),
       title = str_wrap("Adult (18+) smoking prevalence in England, by IMD decile, 2017 to 2023", 80),
       subtitle = str_wrap("1 = most deprived decile, 10 = least deprived decile"),
       caption = "Source: Annual Population Survey, Office for National Statistics\
       LSOA deprivation deciles") +
  bhf_style(textsize = 16) +
  scale_color_bhf("red and light blue")+
  scale_y_continuous(limits = c(0.05,.3),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35)
        ,axis.title.x=element_blank(),
        legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        plot.caption=element_text(size = 12),
        plot.caption.position = "plot")
smoking_plot

#CVDPREVENT plot for hypertension prevalence (%) by deprivation
hyp_prevent_plot <- ggplot(hyp_cvd_prevent, aes(x=IMD_category, y=Value, fill = IMD_category, group = IMD_category)) +
  geom_bar(stat = "identity",position = "dodge")+
  ylab("Prevalence")+
  labs(fill="Index of Multiple Deprivation Quintile",
       title = str_wrap("Age-standardised hypertension prevalence, by IMD quintile", 80),
       subtitle = str_wrap("1 = most deprived quintile, 5 = least deprived quintile"),
       caption = "Source: Cardiovascular Disease Prevention Audit (CVDPREVENT), NHS Digital\
       June 2024 update") +
  bhf_style(textsize = 16) +
  scale_fill_bhf("red and light blue")+
  scale_y_continuous(limits = c(0,0.25),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 18),
        legend.position = "none",
        plot.caption=element_text(hjust = 1, size = 10))
hyp_prevent_plot

#Plor for cholesterol prevalence (%) by deprivation
cholesterol_persons <- cholesterol %>%
  filter(Sex %in% "Persons")

cholesterol_plot <- ggplot(cholesterol_persons, aes(x=Category, y=Percentage, fill = Category, group = Category)) +
  geom_bar(stat = "identity",position = "dodge")+
  ylab("Prevalence")+
  labs(fill="Index of Multiple Deprivation Quintile",
       title = str_wrap("Age-standardised cholesterol prevalence, by IMD quintile", 80),
       subtitle = str_wrap("1 = most deprived quintile, 5 = least deprived quintile"),
       caption = "Source: Health Survey for England, NHS Digital") +
  bhf_style(textsize = 16) +
  scale_fill_bhf("red and light blue")+
  scale_y_continuous(limits = c(0,0.6),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 18),
        legend.position = "none",
        plot.caption=element_text(hjust = 1, size = 10),
        strip.text = element_text(size = 16))
cholesterol_plot


#Plot cholesterol prevalence breakdown by gender: men and women only
cholesterol_gender <- cholesterol %>%
  filter(Sex %in% "Men" | Sex %in% "Women")

cholesterol_plot <- ggplot(cholesterol_gender, aes(x=Category, y=Percentage, fill = Category, group = Category)) +
  facet_wrap(~Sex)+
  geom_bar(stat = "identity",position = "dodge")+
  ylab("Prevalence")+
  labs(fill="Index of Multiple Deprivation Quintile",
       title = str_wrap("Age-standardised cholesterol prevalence, by gender and IMD quintile", 80),
       subtitle = str_wrap("1 = most deprived quintile, 5 = least deprived quintile"),
       caption = "Source: Health Survey for England, NHS Digital") +
  bhf_style(textsize = 16) +
  scale_fill_bhf("red and light blue")+
  scale_y_continuous(limits = c(0,0.7),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 18),
        legend.position = "none",
        plot.caption=element_text(hjust = 1, size = 10),
        strip.text = element_text(size = 16))
cholesterol_plot

#Plot cholesterol prevalence breakdown by gender: all genders
cholesterol$Sex <- factor(cholesterol$Sex, levels = c("Persons",
                                                                "Men",
                                                                "Women")) 
cholesterol_plot <- ggplot(cholesterol, aes(x=Category, y=Percentage, fill = Category, group = Category)) +
  facet_wrap(~Sex)+
  geom_bar(stat = "identity",position = "dodge")+
  ylab("Prevalence")+
  labs(fill="Index of Multiple Deprivation Quintile",
       title = str_wrap("Age-standardised prevalence of raised total cholesterol in England in 2022, by IMD quintile", 80),
       subtitle = str_wrap("1 = most deprived quintile, 5 = least deprived quintile"),
       caption = "Source: Health Survey for England, NHS Digital\
       Note: Data shown for adults (aged 16+)") +
  bhf_style(textsize = 16) +
  scale_fill_bhf("red and light blue")+
  scale_y_continuous(limits = c(0,0.7),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 18),
        legend.position = "none",
        plot.caption=element_text(hjust = 1, size = 10),
        strip.text = element_text(size = 16))
cholesterol_plot

}

#PREVALENCE OF CVD CONDITIONS#
{
# Plot diabetes prevalence (%) by deprivation
diabetes <- prevalence_conditions %>%
    filter(Indicator.Name %in% "Diabetes: QOF prevalence (17+ yrs)") %>%
    filter(Category.Type %in% "County & UA deprivation deciles in England (IMD2019, 4/23 geography)")
  
diabetes_plot <- ggplot(diabetes, aes(x=Time.period, y=Value,colour = Category, group = Category, alpha=Category)) +
  geom_point(size = 2.5) +
  geom_line(size=1.5)+
  ylab("Prevalence")+
  labs(color= str_wrap("Index of Multiple Deprivation Decile",15),
       title = str_wrap("Diabetes prevalence in England, by deprivation decile, 2012/13 to 2023/24", 80),
       subtitle = str_wrap("1 = most deprived decile, 10 = least deprived decile"),
       caption = "Source: Quality Outcomes Framework (QOF), NHS Digital via Office for Health Improvement and Disparities (OHID) Fingertips
                  County & UA deprivation deciles")+
  bhf_style(textsize = 14) +
  scale_color_bhf("red and light blue")+
    scale_y_continuous(limits = c(0.01,.1),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 14),
        legend.title.align = 0,
        plot.caption=element_text(size = 10),
        plot.caption.position = "plot")+
  scale_alpha_manual(values=c(1,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,1), guide=FALSE)
diabetes_plot

# Plot hypertension prevalence (%) by deprivation
hypertension <- prevalence_conditions %>%
  filter(Indicator.Name %in% "Hypertension: QOF prevalence (all ages)") %>%
  filter(Category.Type %in% "District & UA deprivation deciles in England (IMD2019, 4/23 geography)")

hypertension_plot <- ggplot(hypertension, aes(x=Time.period, y=Value,colour = Category, group = Category, alpha=Category)) +
  geom_point(size = 2.5) +
  geom_line(size=1.5)+
  ylab("Prevalence")+
  labs(color= str_wrap("Index of Multiple Deprivation Decile",15),
       title = str_wrap("Hypertension prevalence in England, by deprivation decile, 2012/13 to 2023/24", 80),
       subtitle = str_wrap("1 = most deprived decile, 10 = least deprived decile"),
       caption = "Source: Quality Outcomes Framework (QOF), NHS Digital via Office for Health Improvement and Disparities (OHID) Fingertips\
       District & UA deprivation deciles") +
  bhf_style(textsize = 14) +
  scale_color_bhf("red and light blue")+
  scale_y_continuous(limits = c(.12,.17),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 14),
        legend.title.align = 0,
        plot.caption=element_text(size = 10),
        plot.caption.position = "plot")+
  scale_alpha_manual(values=c(1,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,1), guide=FALSE)
hypertension_plot


# Plot CHD prevalence (%) by deprivation
coronary_heart_disease <- prevalence_conditions %>%
  filter(Indicator.Name %in% "CHD: QOF prevalence (all ages)") %>%
  filter(Category.Type %in% "District & UA deprivation deciles in England (IMD2019, 4/23 geography)")

chd_plot <- ggplot(coronary_heart_disease, aes(x=Time.period, y=Value,colour = Category, group = Category, alpha=Category)) +
  geom_point(size = 2.5) +
  geom_line(size=1.5)+
  ylab("Prevalence")+
  labs(color= str_wrap("Index of Multiple Deprivation Decile",15),
       title = str_wrap("Coronary heart disease prevalence in England, by deprivation decile, 2012/13 to 2023/24", 110),
       subtitle = str_wrap("1 = most deprived decile, 10 = least deprived decile"),
       caption = "Source: Quality Outcomes Framework (QOF), NHS Digital via Office for Health Improvement and Disparities (OHID) Fingertips
                  District & UA deprivation deciles") +
  bhf_style(textsize = 14) +
  scale_color_bhf("red and light blue")+
  scale_y_continuous(limits = c(0.02,.04),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 14),
        legend.title.align = 0,
        plot.caption=element_text(size = 10),
        plot.caption.position = "plot")+
  scale_alpha_manual(values=c(1,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,1), guide=FALSE)
chd_plot


# Plot AF prevalence (%) by deprivation
atrial_fibrillation <- prevalence_conditions %>%
  filter(Indicator.Name %in% "Atrial fibrillation: QOF prevalence (all ages)") %>%
  filter(Category.Type %in% "County & UA deprivation deciles in England (IMD2019, 4/23 geography)")

af_plot <- ggplot(atrial_fibrillation, aes(x=Time.period, y=Value,colour = Category, group = Category, alpha=Category)) +
  geom_point(size = 2.5) +
  geom_line(size=1.5)+
  ylab("Prevalence")+
  labs(color= str_wrap("Index of Multiple Deprivation Decile",15),
       title = str_wrap("Atrial fibrillation prevalence in England, by deprivation decile, 2012/13 to 2023/24", 115),
       subtitle = str_wrap("1 = most deprived decile, 10 = least deprived decile"),
       caption = "Source: Quality Outcomes Framework (QOF), NHS Digital via Office for Health Improvement and Disparities (OHID) Fingertips\
       County & UA deprivation deciles") +
  bhf_style(textsize = 14) +
  scale_color_bhf("red and light blue")+
  scale_y_continuous(limits = c(0.01,.03),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 14),
        legend.title.align = 0,
        plot.caption=element_text(size = 10),
        plot.caption.position = "plot")+
  scale_alpha_manual(values=c(1,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,1), guide=FALSE)
af_plot
}

#CVDPREVENT DATA#
{
# CVDPREVENT plot CVD prevalence (%) by deprivation
cvd_prevent_plot <- ggplot(cvd_cvd_prevent, aes(x=IMD_category, y=Value, fill = IMD_category, group = IMD_category)) +
  geom_bar(stat = "identity",position = "dodge")+
  ylab("Prevalence")+
  labs(fill="Index of Multiple Deprivation Quintile",
       subtitle = str_wrap("1 = most deprived quintile, 5 = least deprived quintile"),
       title = str_wrap("Age-standardised cardiovascular disease prevalence, by IMD quintile", 80),
       caption = "Source: Cardiovascular Disease Prevention Audit (CVDPREVENT), NHS Digital\
       June 2024 update") +
  bhf_style(textsize = 16) +
  scale_fill_bhf("red and light blue")+
  scale_y_continuous(limits = c(0,0.15),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 18),
        legend.position = "none",
        plot.caption=element_text(hjust = 1, size = 10))
cvd_prevent_plot

# CVDPREVENT plot heart failure prevalence (%) by deprivation
hf_prevent_plot <- ggplot(hf_cvd_prevent, aes(x=IMD_category, y=Value, fill = IMD_category, group = IMD_category)) +
  geom_bar(stat = "identity",position = "dodge")+
  ylab("Prevalence")+
  labs(fill="Index of Multiple Deprivation Quintile",
       subtitle = str_wrap("1 = most deprived quintile, 5 = least deprived quintile"),
       title = str_wrap("Age-standardised heart failure prevalence, by IMD quintile", 80),
       caption = "Source: Cardiovascular Disease Prevention Audit (CVDPREVENT), NHS Digital\
       June 2024 update") +
  bhf_style(textsize = 16) +
  scale_fill_bhf("red and light blue")+
  scale_y_continuous(limits = c(0,0.025),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 18),
        legend.position = "none",
        plot.caption=element_text(hjust = 1, size = 10))
hf_prevent_plot


# CVD prevent plot atrial fibrillation prevalence (%) by deprivation
af_prevent_plot <- ggplot(af_cvd_prevent, aes(x=IMD_category, y=Value, fill = IMD_category, group = IMD_category)) +
  geom_bar(stat = "identity",position = "dodge")+
  ylab("Percentage by quintile")+
  labs(fill="Index of Multiple Deprivation Quintile",
       subtitle = str_wrap("1 = most deprived quintile, 5 = least deprived quintile"),
       title = str_wrap("Age-standardised atrial fibrillation prevalence, by IMD quintile", 80),
       caption = "Source: Cardiovascular Disease Prevention Audit (CVDPREVENT), NHS Digital\
       June 2024 update") +
  bhf_style(textsize = 16) +
  scale_fill_bhf("red and light blue")+
  scale_y_continuous(limits = c(0,0.04),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 18),
        legend.position = "none",
        plot.caption=element_text(hjust = 1, size = 10))
af_prevent_plot
}

#AGE-STANDARDISED MORTALITY RATES#
{
  # Plot all age and premature ASDRs by deprivation
persons_asdr_data <- asdr_data %>%
    filter(Sex %in% ("Persons"))%>%
    filter(Time.period.range %in% ("1y"))
  
new_labels <- as_labeller(c("Mortality rate from cardiovascular disease, all ages"="All Ages",
                            "Under 75 mortality rate from cardiovascular disease" = "Premature (Under-75s)"))
  
  
asdr_plot <- ggplot(persons_asdr_data, aes(x=Time.period, y=Value,colour = Category, group = Category)) +
    facet_wrap(~ Indicator.Name,labeller = new_labels)+
    geom_point(size = 2.5, alpha = 0.7) +
    geom_line(size=1.5, alpha = 0.5)+
    ylab("Rate per 100,000 persons")+
    labs(color=str_wrap("Index of Multiple Deprivation Decile",15),
         title = str_wrap("Age-standardised mortality rates from cardiovascular disease in England, 2016-2023", 100),
         caption = "Source: Office for National Statistics via Office for Health Improvement and Disparities (OHID) Fingertips") +
    bhf_style(textsize=16) +
    scale_color_bhf("red and light blue")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35),
          axis.title.x=element_blank(),
          strip.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          plot.caption = element_text(size = 12),
          plot.caption.position = "plot",
          plot.title.position = "plot")
asdr_plot

#Mortality breakdown by sex, single year data
gender_asdr_data <- asdr_data %>%
  filter(Sex %in% "Female" | Sex %in% "Male") %>%
  filter(Time.period.range %in% ("1y"))

gender_labels <- as_labeller(c("Female"="Female", "Male"="Male",
                               "Mortality rate from cardiovascular disease, all ages"="All Ages",
                                "Under 75 mortality rate from cardiovascular disease" = "Premature (Under-75s)"))

sex_asdr_plot <- ggplot(gender_asdr_data, aes(x=Time.period, y=Value,colour = Category, group = Category)) +
  facet_grid(Indicator.Name ~ Sex, scales="free_y", labeller = gender_labels)+
  geom_point(size = 2.5, alpha = 0.7) +
  geom_line(size=1.5, alpha = 0.5)+
  ylab("Rate per 100,000 persons")+
  labs(color=str_wrap("Index of Multiple Deprivation Decile",15),
       title = str_wrap("Age-standardised mortality rates from cardiovascular disease in England, 2016-2023", 80),
       caption = "Source: Office for National Statistics via Office for Health Improvement and Disparities (OHID) Fingertips") +
  bhf_style(textsize=16) +
  scale_color_bhf("red and light blue")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        strip.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        plot.caption = element_text(size = 12),
        plot.caption.position = "plot")
sex_asdr_plot
}

#CARDIAC REHABILITATION# 
{
  cardiac_rehab_plot  <- ggplot(cardiac_rehab_imd, aes(x=Year, y=percentage_uptake, color = IMD_Quintile, group = IMD_Quintile)) +
    geom_point(size = 3.5, alpha = 0.7) +
    geom_line(size=1.5, alpha = 1)+
    ylab("Treatment uptake")+
    labs(color= str_wrap("Index of Multiple Deprivation Quintile",15),
         title = str_wrap("Percentage of cardiac rehabilitation uptake, by IMD quintile, 2019/20 to 2022/23", 80),
         subtitle = str_wrap("1 = most deprived quintile, 5 = least deprived quintile"),
         caption = "Source: National audit of cardiac rehabilitation (NACR) in partnership with York University") +
    bhf_style(textsize = 16) +
    scale_color_bhf("red and light blue")+
    scale_y_continuous(limits = c(0.27,0.45),labels = scales::percent)+
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35)
          ,axis.title.x=element_blank(),
          legend.title = element_text(size = 16),
          legend.title.align = 0.5,
          plot.caption=element_text(size = 10),
          plot.caption.position = "plot",
          plot.title.position = "plot")
  cardiac_rehab_plot 


#Breakdown by ethnicity
cardiac_rehab_ethnicity <- cardiac_rehab %>%
  filter(demographic_variable %in% "Ethnicity")

  cardiac_rehab_ethnicity_plot  <- ggplot(cardiac_rehab_ethnicity, aes(x=Year, y=percentage_uptake, color = IMD_Quintile, group = IMD_Quintile)) +
    geom_point(size = 2.5, alpha = 0.7) +
    geom_line(size=1.5, alpha = 0.5)+
    ylab("Percentage")+
    labs(color= str_wrap("Ethnicity",15),
         title = str_wrap("Percentage of cardiac rehabilitation uptake, by ethnicity", 80),
         caption = "Source: National audit of cardiac rehabilitation (NACR) in partnership with York University") +
    bhf_style(textsize = 16) +
    scale_color_bhf("expanded secondaries")+
    scale_y_continuous(limits = c(0.2,0.8),labels = scales::percent)+
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35)
          ,axis.title.x=element_blank(),
          legend.title = element_text(size = 16),
          legend.title.align = 0.5,
          plot.caption=element_text(hjust = 1, size = 10))
  cardiac_rehab_ethnicity_plot 
  

#Breakdown by gender  
  cardiac_rehab_ethnicity <- cardiac_rehab %>%
    filter(demographic_variable %in% "Gender")
  
  cardiac_rehab_ethnicity_plot  <- ggplot(cardiac_rehab_ethnicity, aes(x=Year, y=percentage_uptake, color = IMD_Quintile, group = IMD_Quintile)) +
    geom_point(size = 2.5, alpha = 0.7) +
    geom_line(size=1.5, alpha = 0.5)+
    ylab("Percentage")+
    labs(color= str_wrap("Gender",15),
         title = str_wrap("Percentage of cardiac rehabilitation uptake, by gender", 80),
         caption = "Source: National audit of cardiac rehabilitation (NACR) in partnership with York University") +
    bhf_style(textsize = 16) +
    scale_color_bhf("expanded secondaries")+
    scale_y_continuous(limits = c(0.2,0.5),labels = scales::percent)+
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35)
          ,axis.title.x=element_blank(),
          legend.title = element_text(size = 16),
          legend.title.align = 0.5,
          plot.caption=element_text(hjust = 1, size = 10))
  cardiac_rehab_ethnicity_plot 
    
}

#ADMISSIONS#
{
# Plot admissions chd by deprivation
admissions_chd <- admissions_data %>%
    filter(Indicator.Name %in% "Hospital admissions due to coronary heart disease")
  
admissions_chd_plot <- ggplot(admissions_chd, aes(x=Time.period, y=Value,colour = Category, group = Category, alpha=Category)) +
  geom_line(size=1.5)+
  ylab("Admissions per 100,000 persons")+
  labs(color= str_wrap("Index of Multiple Deprivation Decile",15),
       subtitle = str_wrap("1 = most deprived decile, 10 = least deprived decile"),
       title = str_wrap("Hospital admissions due to coronary heart disease, 2003/04 to 2022/23", 80),
       caption = "Source: NHS England, Hospital episode statistics (HES)") +
  bhf_style(textsize = 16) +
  scale_color_bhf("red and light blue")+
  scale_y_continuous(limits = c(0,850)) +
  scale_alpha_manual(values=c(1,0.5,0.4,0.3,0.2,0.2,0.3,0.4,0.5,1), guide=FALSE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 16),
        legend.title.align = 0,
        plot.caption=element_text(size = 10),
        plot.caption.position = "plot",
        plot.title.position = "plot")
admissions_chd_plot

# Plot admissions heart failure by deprivation
admissions_hf <- admissions_data %>%
  filter(Indicator.Name %in% "Hospital admissions due to heart failure")

admissions_hf_plot <- ggplot(admissions_hf, aes(x=Time.period, y=Value,colour = Category, group = Category, alpha=Category)) +
  geom_line(size=1.5)+
  ylab("Admissions per 100,000 persons")+
  labs(color= str_wrap("Index of Multiple Deprivation Decile",15),
       subtitle = str_wrap("1 = most deprived decile, 10 = least deprived decile"),
       title = str_wrap("Hospital admissions due to heart failure, 2003/04 to 2022/23", 80),
       caption = "Source: NHS England, Hospital episode statistics (HES)") +
  bhf_style(textsize = 16) +
  scale_color_bhf("red and light blue")+
  scale_y_continuous(limits = c(0,250)) +
  scale_alpha_manual(values=c(1,0.5,0.4,0.3,0.2,0.2,0.3,0.4,0.5,1), guide=FALSE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 16),
        legend.title.align = 0,
        plot.caption=element_text(size = 10),
        plot.caption.position = "plot",
        plot.title.position = "plot")
admissions_hf_plot


# Plot admissions stroke by deprivation
admissions_stroke <- admissions_data %>%
  filter(Indicator.Name %in% "Hospital admissions due to stroke")

admissions_stroke_plot <- ggplot(admissions_stroke, aes(x=Time.period, y=Value,colour = Category, group = Category, alpha=Category)) +
  geom_line(size=1.5)+
  ylab("Admissions per 100,000 persons")+
  labs(color= str_wrap("Index of Multiple Deprivation Decile",15),
       subtitle = str_wrap("1 = most deprived decile, 10 = least deprived decile"),
       title = str_wrap("Hospital admissions due to stroke, 2003/04 to 2022/23", 80),
       caption = "Source: NHS England, Hospital episode statistics (HES)") +
  bhf_style(textsize = 16) +
  scale_color_bhf("red and light blue")+
  scale_y_continuous(limits = c(0,210)) +
  scale_alpha_manual(values=c(1,0.5,0.4,0.3,0.2,0.2,0.3,0.4,0.5,1), guide=FALSE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 16),
        legend.title.align = 0,
        plot.caption=element_text(size = 10),
        plot.caption.position = "plot",
        plot.title.position = "plot")
admissions_stroke_plot
}

#GP SURVEY#
{
#Plot for survey data by deprivation quintiles
survey_plot <- ggplot(survey_2, aes(x=as.factor(Year), y=Value,colour = Category, group=Category)) +
  facet_wrap(~Type)+
  geom_point(size = 2.5, alpha = 0.7) +
  geom_line(size=1.5)+
  ylab("Percentage")+
  labs(color= str_wrap("Index of Multiple Deprivation Quintile",15),
       subtitle = str_wrap("1 = most deprived quintile, 5 = least deprived quintile"),
       title = str_wrap("Reported confidence of adults in managing their health condition(s) in England, by deprivation quintile, 2022 to 2024", 80),
       caption = "Source: NHS England, GP patient survey data") +
  scale_color_bhf("red and light blue")+
  scale_y_continuous(limits = c(0,1),labels = scales::percent)+
  #annotate("label", x=c(2.25,2.25), y=c(0.25,0.75), label = c("Not confident", "Confident"), fontface="bold")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35),axis.title.x=element_blank(),
      strip.text.x = element_text(size = 16),
      axis.title.x.bottom = element_blank(),
      legend.title = element_text(size = 16),
      legend.title.align = 0,
      plot.caption=element_text(size = 10),
      plot.caption.position = "plot",
      plot.title.position = "plot")+
  bhf_style(textsize = 16) 
survey_plot
}

#PRESCRIPTIONS#
{
#Use the below to create a new chart for prescription rates without the breakdown for deprivation
drugs$CHEMICAL_CATEGORY <- gsub("2.11: Antifibrinolytic drugs and haemostatics", "Antifibrinolytic drugs and haemostatics",drugs$CHEMICAL_CATEGORY)
drugs$CHEMICAL_CATEGORY <- gsub("2.1: Positive inotropic drugs", "Positive inotropic drugs",drugs$CHEMICAL_CATEGORY)
drugs$CHEMICAL_CATEGORY <- gsub("2.12: Lipid-regulating drugs", "Lipid-regulating drugs",drugs$CHEMICAL_CATEGORY)
drugs$CHEMICAL_CATEGORY <- gsub("2.2: Diuretics", "Diuretics",drugs$CHEMICAL_CATEGORY)
drugs$CHEMICAL_CATEGORY <- gsub("2.3: Anti-arrhythmic drugs", "Anti-arrhythmic drugs",drugs$CHEMICAL_CATEGORY)
drugs$CHEMICAL_CATEGORY<- gsub("2.4: Beta-adrenoceptor blocking drugs", "Beta-adrenoceptor blocking drugs",drugs$CHEMICAL_CATEGORY)
drugs$CHEMICAL_CATEGORY <- gsub("2.5: Hypertension and heart failure", "Hypertension and heart failure",drugs$CHEMICAL_CATEGORY)
drugs$CHEMICAL_CATEGORY <- gsub("2.6: Nitrates, calcium-channel blockers & other antianginal drugs", "Nitrates, calcium-channel blockers & other antianginal drugs",drugs$CHEMICAL_CATEGORY)
drugs$CHEMICAL_CATEGORY <- gsub("2.7: Sympathomimetics", "Sympathomimetics",drugs$CHEMICAL_CATEGORY)
drugs$CHEMICAL_CATEGORY <- gsub("2.8: Anticoagulants and protamine", "Anticoagulants and protamine",drugs$CHEMICAL_CATEGORY)
drugs$CHEMICAL_CATEGORY <- gsub("2.9: Antiplatelet drugs", "Antiplatelet drugs",drugs$CHEMICAL_CATEGORY)

#2 drugs removed: sympathomimetics and antifibrinolytics and haemostatics

nodecile_drugs <- drugs %>%
  select(c(CHEMICAL_CATEGORY, year,rate))%>%
  filter(CHEMICAL_CATEGORY %in% c("Lipid-regulating drugs", 
                                  "Positive inotropic drugs",
                                  "Diuretics",
                                  "Anti-arrhythmic drugs",
                                  "Beta-adrenoceptor blocking drugs",
                                  "Hypertension and heart failure",
                                  "Nitrates, calcium-channel blockers & other antianginal drugs",
                                  "Anticoagulants and protamine",
                                  "Antiplatelet drugs")) %>%
  group_by(CHEMICAL_CATEGORY, year) %>%
  summarise(total_chemical_category_rate=sum(rate))%>%
  mutate(CHEMICAL_CATEGORY= str_wrap(CHEMICAL_CATEGORY, width=20))

ten_colours = c("#FF0030","#ffc400","#5fd1ff","#500AB4", "#ff7c00","#018d02", "#f14cc1", "#afd179","#724b06", "#1c83ff")

nodecile_drugs_plot <- ggplot(nodecile_drugs, aes(x=as.factor(year), y=total_chemical_category_rate,colour = CHEMICAL_CATEGORY, group = CHEMICAL_CATEGORY)) +
  geom_point(size = 2.5) +
  geom_line(size=1.5)+
  ylab("Rate per 1,000 persons")+
  labs(color="Drug type",
      title = str_wrap("Rate of prescriptions for cardiovascular conditions, by drug type", 80),
       caption = "Source: English prescribing data, NHS Business Services Authority (NHSBSA)") +
  bhf_style(textsize = 16) +
  scale_color_manual(values = ten_colours, name = "Drug type")+
  scale_y_continuous(labels = scales::comma)+
  guides(fill = guide_legend(byrow = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size=16),
        plot.caption=element_text(size = 10),
        plot.caption.position = "plot",
        legend.key.spacing.y=unit(0.5,"cm"),
        strip.text.x = element_text(size = 16))
nodecile_drugs_plot

#New faceted chart for prescripions
drugs <- drugs %>%
  filter(CHEMICAL_CATEGORY %in% c("Antifibrinolytic drugs and haemostatics",
                                  "Lipid-regulating drugs",
                                  "Positive inotropic drugs",
                                  "Diuretics",
                                  "Anti-arrhythmic drugs",
                                  "Beta-adrenoceptor blocking drugs",
                                  "Hypertension and heart failure",
                                  "Nitrates, calcium-channel blockers & other antianginal drugs",
                                  "Sympathomimetics",
                                  "Anticoagulants and protamine",
                                  "Antiplatelet drugs"))
drugs <- drugs %>%
  filter(CHEMICAL_CATEGORY %in% c("Lipid-regulating drugs",
                                  "Positive inotropic drugs",
                                  "Diuretics",
                                  "Anti-arrhythmic drugs",
                                  "Beta-adrenoceptor blocking drugs",
                                  "Hypertension and heart failure",
                                  "Nitrates, calcium-channel blockers & other antianginal drugs",
                                  "Anticoagulants and protamine",
                                  "Antiplatelet drugs"))

all_drugs_plot <- ggplot(drugs, aes(x=as.factor(year), y=rate,colour = decile, group = decile, alpha=decile)) +
  facet_wrap(~CHEMICAL_CATEGORY,scales = "free_y", labeller = label_wrap_gen())+
  geom_point(size = 2.5) +
  geom_line(size=1.5)+
  ylab("Rate per 1,000 persons")+
  labs(color= str_wrap("Index of Multiple Deprivation Decile",50),
       title = str_wrap("Prescribing rate of cardiovascular drugs, by BNF drug type and IMD decile, 2018 to 2023", 80),
       subtitle = str_wrap("1 = most deprived decile, 10 = least deprived decile"),
       caption = "Source: English prescribing data, NHS Business Services Authority (NHSBSA)") +
  bhf_style(textsize = 18) +
  scale_color_bhf("red and light blue")+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 18),
        legend.title.align = 0.5,
        plot.caption=element_text(size = 12),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.title = element_text(size = 20),
        legend.position = "bottom",
        legend.direction = "vertical",
        strip.text.x = element_text(size = 16))+
  scale_alpha_manual(values=c(0.8,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.8), guide=FALSE) +
  guides(colour = guide_legend(nrow = 1))
all_drugs_plot
}

#LIFE EXPECTANCY#
{
life_expectancy <- life_expectancy %>%
  mutate(new_percents= percentage_contribution_to_the_gap/100)

life_expectancy$Category <- factor(life_expectancy$Category, levels=c("Circulatory",
                                                                         "Cancer",
                                                                         "Respiratory",
                                                                         "Digestive",
                                                                         "External causes",
                                                                         "Mental and behavioural",
                                                                         "COVID-19",
                                                                         "Deaths under 28 days",
                                                                         "Other"))

life_plot <- ggplot(life_expectancy, aes(x=Sex, y=new_percents, fill = Category, group = Category)) +
  facet_wrap(~Time_period)+
  geom_bar(stat = "identity",position = "stack")+
  geom_label(aes(label = scales::percent(round(new_percents,digits = 3),accuracy = 1.1)),position = position_stack(vjust = 0.5),fill="white",size=4)+
  #geom_text(size = 3, label = scales::percent(), position = position_stack(vjust = 0.5))+
  ylab("Percentage contribution (%)")+
  labs(title = str_wrap("Life expectancy gap breakdown between the most and least deprived quintiles in England, by cause of death 2017-2019 and 2020-2021", 80),
       caption = "Source: Office for Health Improvement and Disparities based on ONS death registration data (provisional for 2021)\
       and 2020 mid-year population estimates, and Department for Levelling Up,\
       Housing and Communities Index of Multiple Deprivation, 2019,\
       Breakdown of the life expectancy gap.") +
  bhf_style(textsize = 16) +
  scale_fill_bhf("expanded secondaries")+
  scale_y_continuous(limits = c(0,1.01),labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.35),
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        plot.caption=element_text(size = 10),
        plot.caption.position = "plot",
        strip.text = element_text(size = 14))
life_plot
}

############### loading required packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidylog)

###### loading data set
df <- read_csv2("./weekly_wage.csv")
glimpse(df)


## The columns require parsing because the numbers have the dollar symbol,
## We need to tell R they are numbers
df <- df |> 
  mutate(Public_school_teachers = parse_number(Public_school_teachers),
         Other_college_grads = parse_number(Other_college_grads))

## The last step is to convert the data set to a long format that can be used for the plot
df <- df |> 
  pivot_longer(data = _, 
               cols = c(Public_school_teachers, Other_college_grads), 
               names_to = "teacher_graduate",
               values_to = "wages")


##################################### PLOT #####################################
teacher_pay <-
  ggplot(data = df, mapping = aes(x = Year, y = wages, group = teacher_graduate)) +
  geom_line(mapping = aes(colour = teacher_graduate), size = 1.5, lineend = "round") +
  ## add some labels to the lines  
  geom_text(mapping = aes(x = 1978, y = 1364, label = "$1,364"), vjust = -0.8) +
  geom_text(mapping = aes(x = 1992, y = 1536, label = "$1,536"), vjust = -0.8) +
  geom_text(mapping = aes(x = 1995, y = 1564, label = "$1,564"), vjust = -2.0, hjust = -0.003) +
  geom_text(mapping = aes(x = 2019, y = 2009, label = "$2,009"), vjust = -0.8, hjust = -0.3) +
  geom_text(mapping = aes(x = 1978, y = 1052, label = "$1,052"), vjust = -0.8) +
  geom_text(mapping = aes(x = 1992, y = 1250, label = "$1,250"), vjust = -0.8) +
  geom_text(mapping = aes(x = 1995, y = 1319, label = "$1,319"), vjust = -0.8, hjust = -0.05) +
  geom_text(mapping = aes(x = 2020, y = 1348, label = "$1,348"), vjust = -0.8, hjust = 0.05) +
  ## force the scale to start from 750 and end at 2250
  scale_y_continuous(breaks = seq(from = 750, to = 2250, by = 250),
                     labels = c(750,"1,000","1,250","1,500","1,750","2,000","$2,250")) +
  expand_limits(y = c(750, 2250)) +
  ## specify the colour the lines should be
  scale_colour_discrete(type = c("darkblue", "skyblue"), 
                        ## change names of legends to "Other college grads" and "Public school teachers"
                        labels = c("Other college grads", "Public school teachers"), 
                        ## switch the position of the legends such that Public school teacher appears first 
                        guide = guide_legend(reverse = TRUE)) +
  
  ## add plot title, subtitle and caption
  labs(title = "Teachers’ weekly wages have remained relatively flat for \n25 years",
       subtitle = "Average weekly wages of public school teachers and other college \ngraduates, 1979–2021",
       caption = "Notes: Figure shows average weekly wages (2021$) of public school teachers (elementary, middle, and 
secondary) and other college graduate (nonteacher) peers. Data points for 1994 and 1995 are unavail-
able; dotted lines represent interpolated data. See Allegretto and Mishel 2019, Appendix A, for more
details on data and methodology.

Source: Author’s analysis of Current Population Survey Outgoing Rotation Group data accessed via the 
EPI Current Population Survey Extracts, Version1.0.29 (EPI 2022a), https://microdata.epi.org.") +
  
  ## Make background white
  theme(panel.background = element_rect(fill = "white"),
        ## remove grids
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        ## turn y-axis grid to dashed
        panel.grid.major.y = element_line(linetype = "dotted", colour = "grey", size = 0.7),
        ## remove y-axis ticks
        axis.ticks.y = element_blank(),
        ## remove axis elements
        axis.title = element_blank(),
        ## change axis texts' colour to black
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black"),
        ## remove legend title
        legend.title = element_blank(),
        ## Push legends inside the plot
        legend.position = c(0.2,0.89),
        ## remove legend keys
        legend.key = element_blank(),
        ## make legend labels bigger
        legend.text = element_text(size = 12),
        ## make legend background plain
        legend.background = element_blank(),
        ## make plot title bigger
        plot.title = element_text(size = 19.9, face = "bold"),
        ## give spacing between plot subtitle and graph
        plot.subtitle = element_text(margin = margin(b = 17, t = 7), size = 16),
        ## make long plot title and caption align to the left
        plot.title.position = "plot",
        plot.caption.position = "plot",
        ## increase the font size for the caption and align to left
        plot.caption = element_text(hjust = 0, size = 11.3, margin = margin(b = 25, t = 25)),
        ## add straight visible line to x axis
        axis.line.x = element_line(colour = "grey"),
        axis.ticks.x = element_line(colour = "grey"),
        ## increasing the length of x axis 
        axis.ticks.length.x = unit(0.19, "cm")
  ) 


### Saving the plot
ggsave(filename="teacher_pay.png", plot=teacher_pay, width =7.6 , height=7, units="in", bg="white")

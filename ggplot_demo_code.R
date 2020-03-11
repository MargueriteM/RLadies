#################################################################################
#                         R-Ladies, El Paso, 11 March 2020                      #
# This is a starter of some basic ggplot features and how to customise figures. #
#                        by: Marguerite Mauritz                                 #
#################################################################################

#There are lots of resources online.

# R cheatsheets:  
# [In english](https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf)  
# [En español](https://rstudio.com/wp-content/uploads/2015/04/ggplot2-spanish.pdf) 

# [Cookbooks](http://www.cookbook-r.com/Graphs/)

# Hadley Wickham's [Tidyverse](https://www.tidyverse.org)

### Here, I have combined dplyr to prepare the data andd ggplot for graphing (*obviously*) 
# Inspiration also came from [this tutorial](http://www.rebeccabarter.com/blog/2017-11-17-ggplot2_tutorial/) by Rebecca Barter. 

## Some basics

# ggplot uses layered grammar that consists of **data**, **aesthetics** that describe the positioning and visuals of data, and 
# **geometries** that describe the geometric objects used to display the data. 

#### So, to build a ggplot you always need to define the basics: 
# * data
# * aes (aesthetics: x, y, colour group, shape group)
# * geom (geometric objects: lines, points, boxes, bars, histograms, smoothers, etc)


## Get started: 

### First, load R libraries
library(tidyverse)
library(lubridate)

### Second, import data, transform to long format, and subset for the basic start
# *Don't worry about the details for now*

# Import data in wide format
setwd ()
data.wide <- read.csv("Precip_SoilMoisture_USJo1_2018_2019_wide.csv")
data.wide <- data.wide %>% mutate(date_time = ymd_hms(date_time))

## alternative (if you don't have lubridate)
# load("Precip_SoilMoisture_USJo1_2018_2019_wide.R")

# transform to long format
data.long.pre <- data.wide %>%
  pivot_longer(-date_time, names_to = "variable.id", values_to = "value")

# split variable.id into meaningful descriptors
data.long <- data.long.pre %>% 
  mutate(variable = sapply(strsplit(as.character(variable.id),"_"),"[",1),
         vegetation = sapply(strsplit(as.character(variable.id),"_"),"[",2),
         depth = sapply(strsplit(as.character(variable.id),"_"),"[",3)) %>%
  filter(vegetation %in% c("BARE","LATR","rain"))

# subset data  to explore some basic ggplot features
data.basic <- data.long %>% filter (variable == "VWC" & vegetation == "LATR" & depth %in% c("10","30") & year(date_time)==2018)


### Third, start making some figures
# Remember, ggplot needs you to define:  
# * data
# * aes
# * geom

##### The basic structure is this: 
# ggplot(**data**, **aes**(x,y, colour = , shape = )) +  
#   **geom**_point ()

# Map the data and aesthetics:
ggplot(data.basic, aes(date_time, value))


# Add the geom:
ggplot(data.basic, aes(date_time, value))+
  geom_point(size=0.5)


# It looks like there are two things going on. Let's add a colour to distinguish groups:
ggplot(data.basic, aes(date_time, value, colour = depth))+
geom_point(size=0.5)


# Cool, now I can see I have data at two different depths!

# Now, play with different geoms. Change the geom_point to a line:
ggplot(data.basic, aes(date_time, value, colour = depth))+

# define different geom here: 
geom_ ()



# Instead of a colour you could specify the line type in the aes. Graph as a line and change the line type
ggplot(data.basic, aes(date_time, value, *change linetype here* = depth))+

# define different geom here: 
geom_ ()


# Maybe you want to see how the data values are distributed.
# Graph the data as a density plot  
# Note: Density plots use only an x variable, no y
ggplot(data.basic, aes(value, colour = depth))+
geom_density()


# When you change the geom display, often you have to change the aes to match the geom style
# For a density plot a fill can be nicer than a colour.

ggplot(data.basic, aes(value, fill = depth))+
geom_density(alpha=0.5)

# Notice how fill changed the area colour (the fill) and we lost the colour of the line.
# If we want both fill and line colour:
ggplot(data.basic, aes(value, fill = depth, colour = depth))+
geom_density(alpha=0.5)



# Ok, cool. Now let's get more involved. 
# Let's learn a bit more about this data, distinguish between wide and long format, and make more graphs!

### By the end, you will have code to customise your ggplot and make figures like this: 

data.long %>%
filter(variable == "VWC" & !(vegetation == "BARE" & depth=="5") & depth != "5" ) %>%
mutate(depth.order = factor(depth, levels = c("5","10","20","30"))) %>%
mutate(veg.labels = ifelse(vegetation=="BARE", "Bare ground","Creosote")) %>%

ggplot(.,aes(date_time, value, colour=depth.order))+
geom_line()+
labs(title="Soil Moisture for Bare ground and Creosote", x="Date & Time", y="Water Content (%)", colour = "Depth (cm)")+
scale_colour_manual(values=(c("10"="mediumblue",
"20"="deepskyblue3",
"30"="darkslategray3")))+
facet_grid(veg.labels~., scales = "free_y")+
theme(axis.text.y = element_text(size=10,margin=unit(c(1,2,1,2),"mm")), # margin:adjust text distance from axis
axis.text.x = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
axis.title.y = element_text(size=12),
axis.title.x = element_text(size=12),
axis.ticks.length=unit(-1.0,"mm"),
legend.key.size=unit(4,"mm"),
legend.key = element_rect(fill=NA, colour=NA),
legend.title =element_text(size=10),
legend.text = element_text(size=8),
legend.position = c(0.09,0.37), # or: "top","bottom","left","right"
# legend.justification=c(0.1,0.5),
strip.text.y = element_text(size=14),
strip.background = element_rect(colour="white",fill="white"),
panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
panel.border = element_rect(colour="black", fill=NA),
panel.grid = element_blank(),
plot.margin=unit(c(2,1,1,1),"mm") #top,right,bottom,left)
)


## About the data

# The data we are using was collected by the [Systems Ecology Lab at UTEP](https://selutep.squarespace.com/chihuahuan-desert).  
# The data show soil moisture and precipitation (rainfall) at a research site in the Chihuhahuan Desert at the Jornada Experimental Range. 

# This data shows how different sizes and distribution of rainfall events affect the soil moisture profile (5-30cm depth).

# The rainfall data is collected with a rainguage and measured in mm, the soil moisture data is measured as a percentage. 

# The soil moisture is measured in bare ground and under Creosote (Larrea tridendata = LATR) because the plant canopy and root structure may impact how much water reaches the soil and the pathway that water follows into the ground. The canopy intercepts rain drops and can slow rain drops to reduce water loss via surface runoff. In adddition, the presence of root channels under the creosote may help water flow into the ground instead of running off the surface. 


# Back to the data for a moment because it's helpful to understand data structure when using ggplot. 
### We imported data in wide format and then converted to long format:
### What's the deal?

#### View the data in wide format
# view top of data (default, n=6 first rows)
head(data.wide)

#### ...and all the column names
# view top of data (default, n=6 first rows)
colnames(data.wide)

## Plot data in wide format
ggplot(data.wide)+
  geom_line(aes(date_time, VWC_BARE_5), colour="lightblue")+
  geom_line(aes(date_time, VWC_BARE_10), colour="darkblue")



### Then we transformed the data to long format. WHAT?!
# using this code: 
# data.long.pre <- data.wide %>%
#  pivot_longer(-date_time, names_to = "variable.id", values_to = "value")

#### Let's look at that 'loooooong' format data
head(data.long.pre)

#### *This is still hard to use. Let's split variable ID into meaningful descriptors:*
# we used this code:
# data.long <- data.long.pre %>% 
#   mutate(variable = sapply(strsplit(as.character(variable.id),"_"),"[",1),
#          vegetation = sapply(strsplit(as.character(variable.id),"_"),"[",2),
#          depth = sapply(strsplit(as.character(variable.id),"_"),"[",3)) %>%
#   filter(vegetation %in% c("BARE","LATR","rain"))

#### Let's look at the long format now. 
head(data.long)

#### Great. We made a variable, depth, and vegetation column. What's in those columns?
# Look at levels in variable, depth, and vegetation
levels(factor(data.long$variable.id))
levels(factor(data.long$variable))
levels(factor(data.long$vegetation))
levels(factor(data.long$depth))

### Ok. Awesome. I think I understand the data a bit better. 
### *Let's get back to figures!*

# Figures from long format data:

#### Plot rainfall data
data.long %>%
  filter(variable == "P") %>%
  ggplot(.,aes(date_time, value))+
  geom_line()


#### Plot soil moisture data
data.long %>%
  filter(variable == "VWC") %>%
  ggplot(.,aes(date_time, value))+
  geom_line()

### *Oh, oops. This looks weird. I forgot we had different moisture and depth groups!  Let's add that info.*
#### Add vegetation and depth detail to soil moisture plot
data.long %>%
  filter(variable == "VWC") %>%
  ggplot(.,aes(date_time, value, colour=vegetation))+
  geom_line(aes(linetype=depth))


### Meh, still kinda ugly and hard to read. 
#### *Make a nicer vegetation and depth plot for soil moisture*
data.long %>%
  filter(variable == "VWC") %>%
  ggplot(.,aes(date_time, value, colour=depth))+
  geom_line()+
  facet_grid(vegetation~.)

### Scale y-axes to each data facet using scales="free_y"
data.long %>%
  filter(variable == "VWC") %>%
  ggplot(.,aes(date_time, value, colour=depth))+
  geom_line()+
  facet_grid(vegetation~., scales = "free_y")


### The data at 5cm depth in bare areas looks wacky, remove that
data.long %>%
  filter(variable == "VWC" & !(vegetation == "BARE" & depth=="5")) %>%
  ggplot(.,aes(date_time, value, colour=depth))+
  geom_line()+
  facet_grid(vegetation~., scales = "free_y")

### Make the figure prettier with default theme (theme_bw)
data.long %>%
  filter(variable == "VWC" & !(vegetation == "BARE" & depth=="5")) %>%
  ggplot(.,aes(date_time, value, colour=depth))+
  geom_line()+
  facet_grid(vegetation~., scales = "free_y")+
  theme_bw()

### Modify axes with the labs command
data.long %>%
  filter(variable == "VWC" & !(vegetation == "BARE" & depth=="5")) %>%
  ggplot(.,aes(date_time, value, colour=depth))+
  geom_line()+
  labs(title="Soil Moisture", x="Date & Time", y="Water Content (%)")+
  facet_grid(vegetation~., scales = "free_y")+
  theme_bw()


### Argh!!! R is ordering my data wrong. Why is 5 after 10, 20, and 30!?
# Fix data order with user-specified levels by adding a new column with mutate function
data.long %>%
  filter(variable == "VWC" & !(vegetation == "BARE" & depth=="5")) %>%
  mutate(depth.order = factor(depth, levels = c("5","10","20","30"))) %>%
  
  ggplot(.,aes(date_time, value, colour=depth.order))+
  geom_line()+
  labs(title="Soil Moisture", x="Date & Time", y="Water Content (%)")+
  facet_grid(vegetation~., scales = "free_y")+
  theme_bw()



### There isn't even any 5cm data. Let's remove it.
# Clean up by dropping data that isn't there by adding an aditional filter
data.long %>%
filter(variable == "VWC" & !(vegetation == "BARE" & depth=="5") & depth != "5") %>%
#  droplevels () %>%
mutate(depth.order = factor(depth, levels = c("5","10","20","30"))) %>%

ggplot(.,aes(date_time, value, colour=depth.order))+
geom_line()+
facet_grid(vegetation~., scales = "free_y")+
theme_bw()

### But R is still using column names and keeps changing the colours around!!
# Add meaningful labels and fix colours using scale_colour_manual  
# [colour examples](http://sape.inf.usi.ch/quick-reference/ggplot2/colour)

data.long %>%
filter(variable == "VWC" & !(vegetation == "BARE" & depth=="5") & depth != "5") %>%
#  droplevels () %>%
mutate(depth.order = factor(depth, levels = c("5","10","20","30"))) %>%
mutate(veg.labels = ifelse(vegetation=="BARE", "Bare ground","Creosote")) %>%

ggplot(.,aes(date_time, value, colour=depth.order))+
geom_line()+
labs(title="Soil Moisture for Bare ground and Creosote", x="Date & Time", y="Water Content (%)", colour = "Depth (cm)")+
scale_colour_manual(values=(c("10"="mediumblue",
"20"="deepskyblue3",
"30"="darkslategray3")))+
facet_grid(veg.labels~., scales = "free_y")+
theme_bw()

### That grey in the side bars is still ugly, add a custom theme piece
data.long %>%
filter(variable == "VWC" & !(vegetation == "BARE" & depth=="5") & depth != "5") %>%
#  droplevels () %>%
mutate(depth.order = factor(depth, levels = c("5","10","20","30"))) %>%
mutate(veg.labels = ifelse(vegetation=="BARE", "Bare ground","Creosote")) %>%

ggplot(.,aes(date_time, value, colour=depth.order))+
geom_line()+
labs(title="Soil Moisture for Bare ground and Creosote", x="Date & Time", y="Water Content (%)", colour = "Depth (cm)")+
scale_colour_manual(values=(c("10"="mediumblue",
"20"="deepskyblue3",
"30"="darkslategray3")))+
facet_grid(veg.labels~., scales = "free_y")+
theme_bw()+
theme( strip.background = element_rect(colour="white",fill="white"))

### Theme allows you to change almost everything! If you use just theme()
data.long %>%
filter(variable == "VWC" & !(vegetation == "BARE" & depth=="5") & depth != "5") %>%
#  droplevels () %>%
mutate(depth.order = factor(depth, levels = c("5","10","20","30"))) %>%
mutate(veg.labels = ifelse(vegetation=="BARE", "Bare ground","Creosote")) %>%

ggplot(.,aes(date_time, value, colour=depth.order))+
geom_line()+
labs(title="Soil Moisture for Bare ground and Creosote", x="Date & Time", y="Water Content (%)", colour = "Depth (cm)")+
scale_colour_manual(values=(c("10"="mediumblue",
"20"="deepskyblue3",
"30"="darkslategray3")))+
facet_grid(veg.labels~., scales = "free_y")+
theme(axis.text.y = element_text(size=10,margin=unit(c(1,2,1,2),"mm")), # margin:adjust text distance from axis
axis.text.x = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
axis.title.y = element_text(size=12),
axis.title.x = element_text(size=12),
axis.ticks.length=unit(-1.0,"mm"),
legend.key.size=unit(4,"mm"),
legend.key = element_rect(fill=NA, colour=NA),
legend.title =element_text(size=10),
legend.text = element_text(size=8),
legend.position = c(0.09,0.37), # or: "top","bottom","left","right"
# legend.justification=c(0.1,0.5),
strip.text.y = element_text(size=14),
strip.background = element_rect(colour="white",fill="white"),
panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
panel.border = element_rect(colour="black", fill=NA),
panel.grid = element_blank(),
plot.margin=unit(c(2,1,1,1),"mm") #top,right,bottom,left)
)


### Save the plot using ggsave
# to save, assign the plot to an object and then use ggsave
p<-data.long %>%
filter(variable == "VWC" & !(vegetation == "BARE" & depth=="5") & depth != "5") %>%
#  droplevels () %>%
mutate(depth.order = factor(depth, levels = c("5","10","20","30"))) %>%
mutate(veg.labels = ifelse(vegetation=="BARE", "Bare ground","Creosote")) %>%

ggplot(.,aes(date_time, value, colour=depth.order))+
geom_line()+
labs(title="Soil Moisture for Bare ground and Creosote", x="Date & Time", y="Water Content (%)", colour = "Depth (cm)")+
scale_colour_manual(values=(c("10"="mediumblue",
"20"="deepskyblue3",
"30"="darkslategray3")))+
facet_grid(veg.labels~., scales = "free_y")+
theme(axis.text.y = element_text(size=10,margin=unit(c(1,2,1,2),"mm")), # margin:adjust text distance from axis
axis.text.x = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
axis.title.y = element_text(size=12),
axis.title.x = element_text(size=12),
axis.ticks.length=unit(-1.0,"mm"),
legend.key.size=unit(4,"mm"),
legend.key = element_rect(fill=NA, colour=NA),
legend.title =element_text(size=10),
legend.text = element_text(size=8),
legend.position = c(0.09,0.37), # or: "top","bottom","left","right"
strip.text.y = element_text(size=14),
strip.background = element_rect(colour="white",fill="white"),
panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
panel.border = element_rect(colour="black", fill=NA),
panel.grid = element_blank(),
plot.margin=unit(c(2,1,1,1),"mm") #top,right,bottom,left)
)

ggsave(filename="sampleplot", plot=p, device="pdf")


### Show only data in July and August 2019 using scale_x_datetime
data.long %>%
filter(!(vegetation == "BARE" & depth=="5") & (is.na(depth) | depth %in% c("10","20","30"))) %>%
# mutate(depth.order = factor(depth, levels = c("5","10","20","30","NA"))) %>%
mutate(veg.labels = ifelse(vegetation=="rain", "Rain", ifelse(vegetation=="BARE", "Bare ground","Creosote"))) %>%
mutate(depth = ifelse(vegetation=="rain","rain",depth)) %>%
ggplot(.,aes(date_time, value, colour=depth))+
geom_line()+
scale_x_datetime(limits=c(as.POSIXct("2019-07-01 00:00:00"),as.POSIXct("2019-08-31 00:00:00")))+
labs(title="Soil Moisture and Precipitation", x="Date & Time", y="Rain (mm), Water Content (%)", colour = "Depth (cm)")+
scale_colour_manual(values=(c("10"="mediumblue",
"20"="deepskyblue3",
"30"="darkslategray3",
"rain" = "black")))+
facet_grid(paste(variable,veg.labels)~., scales = "free_y")+
theme(axis.text.y = element_text(size=10,margin=unit(c(1,2,1,2),"mm")), # margin:adjust text distance from axis
axis.text.x = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
axis.title.y = element_text(size=12),
axis.title.x = element_text(size=12),
axis.ticks.length=unit(-1.0,"mm"),
legend.key.size=unit(4,"mm"),
legend.key = element_rect(fill=NA, colour=NA),
legend.title =element_text(size=10),
legend.text = element_text(size=8),
strip.text.y = element_text(size=10),
strip.background = element_rect(colour="white",fill="white"),
panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
panel.border = element_rect(colour="black", fill=NA),
panel.grid = element_blank(),
plot.margin=unit(c(2,1,1,1),"mm") #top,right,bottom,left)
)


# Getting really fancy by graphing data from multiple data frames
# Let's calculate an average soil moisture for all depths and plot it on top of the individual depths
vwc.mean <- data.long %>%
  filter(variable == "VWC" & !(vegetation == "BARE" & depth=="5") & depth != "5") %>%
  mutate(veg.labels = as.factor(ifelse(vegetation=="BARE", "Bare ground","Creosote")),
         date = as.Date(date_time)) %>%
  group_by(date,veg.labels) %>%
  summarise(vwc.mean = mean(value))

summary(vwc.mean)


### Overlay data from the raw measurements and averages in one figure
# when you put the data inside the ggplot() command then you have defined a global data frame that will apply to all other parts
# of the figure. 
# if you want to graph different data in each geom command then you can specify data within the geom_ () itself
# add two geom_line() components and define different data for each. 
ggplot()+
  geom_line(data = data.long %>%
              filter(variable == "VWC" & !(vegetation == "BARE" & depth=="5") & depth != "5") %>%
              mutate(depth.order = factor(depth, levels = c("5","10","20","30"))) %>%
              mutate(veg.labels = ifelse(vegetation=="BARE", "Bare ground","Creosote")),
            
            aes(date_time, value, colour=depth.order), size=1)+
  geom_line(data=vwc.mean, aes(as.POSIXct(date), vwc.mean, colour="mean"),size=2)+
  labs(title="Soil Moisture for Bare ground and Creosote", x="Date & Time", y="Water Content (%)", colour = "Depth (cm)")+
  scale_colour_manual(values=(c("10"="mediumblue",
                                "20"="deepskyblue3",
                                "30"="darkslategray3",
                                "mean"="black")))+
  facet_grid(veg.labels~., scales = "free_y")+
  theme(axis.text.y = element_text(size=10,margin=unit(c(1,2,1,2),"mm")), # margin:adjust text distance from axis
        axis.text.x = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.ticks.length=unit(-1.0,"mm"),
        legend.key.size=unit(4,"mm"),
        legend.key = element_rect(fill=NA, colour=NA),
        legend.title =element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = c(0.09,0.37), # or: "top","bottom","left","right"
        # legend.justification=c(0.1,0.5),
        strip.text.y = element_text(size=14),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,1,1),"mm") #top,right,bottom,left)
  )


# The wide data format does have some uses for graphing
# For examples, it's much easier to graph relationships
ggplot(data.wide, aes(VWC_LATR_10, VWC_BARE_10))+
  geom_point()+
  theme_bw()


library(tidyverse);library(car);library(mosaic);library(dplyr);library(ggplot2)
library(nycflights13);library(ggplot2);library(knitr);library(usethis)
use_git_config(user.name = "yooj", user.email = "yoo18001@byui.edu")
#exampels from chapter 4 and 1
x <- sin(pi/9)
y <- 10^2
x+y
seq(-2, 10, length.out = 5)
z <- "Citrus"
glimpse(flights)
kable(airlines)

#I learned several functions such as glimpse and get familier with ggplot.

# ggplot 
### ggplot(data = ___, mapping=aes(x=sepal.length,y=sepal.width))+
###   geom_ ***() #*** = type of graph

#ggplot example - from class
ggplot(data=iris, mapping=aes(x=Sepal.Length,y=Sepal.Width, 
  color = Species, shape= Species, size = Petal.Length))+
  geom_point()+
facet_wrap(~Species)

#making chart
dat <- read_csv("https://byuistats.github.io/M335/data/rcw.csv", 
    col_types = cols(Semester_Date = col_date(format = "%m/%d/%y"), 
     Semester = col_factor(levels = c("Winter", "Spring", "Fall"))))
head(dat)

dat$year_semester <- paste(dat$Year,dat$Semester)
 dat <- dat %>% arrange(Semester_Date) %>% group_by(Department) 
  
ggplot(data= dat, aes(x= Semester_Date, y=Count, color = Department)) +
  geom_point(size=1.4) +
  geom_line(size = 1.2) 





# No way this works

# OMG !!! 😂😂😁😁😎😎


# This is so cool

rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/myOwnFunctions.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/desc.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.nas.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/compare.independent.samples.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/freq_expected.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.summary.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.expectedVector.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/desc.numeric.R", encoding = 'UTF-8')



# Let's make that function
DATA<-datasets::mtcars
View(DATA)
DATA<-getMe.nas(DATA)
x = DATA$mpg

compare.samples(x,y,DEBUG=T,na.rm=FALSE)
DATA$vs <- factor(DATA$vs,levels = c(0,1),labels = c("V-shaped","Straight"))


# Let's try with elevator DATA
attributes(split(DATA$DV_SPEED_FPM,DATA$`Device Type`))$names
attributes(split(datasets::mtcars$mpg,datasets::mtcars$vs))$names
attributes(split(datasets::mtcars$mpg,datasets::mtcars$vs))$names
attributes(DATA$`Device Type`)
as.numeric(DATA$`Device Type`)
as.vector(attributes(split(DATA$DV_SPEED_FPM,DATA$`Device Type`))$names)
# DATA$`Device Type` <- factor(DATA$`Device Type`,levels = c(attributes(split(DATA$DV_SPEED_FPM,DATA$`Device Type`))$names),
                             # labels = c("V-shaped","Straight"...))

DATA$`Device Type` <- factor(DATA$`Device Type`) # R Studio is already smart...
# Doesn't matter how many levels you have...
what_is_this <- split(DATA$DV_SPEED_FPM,DATA$`Device Type`)

class(what_is_this)

attributes(what_is_this)$names[1]
what_is_this[2]
class(what_is_this[1]$`Dumbwaiter (D)`)
class(what_is_this[[1]])

?list
desc.numeric
what_is_this_cars <- split()
# I want you to see that this is the same for datasets::mtcars
vector_of_names <- attributes(what_is_this)$names[3]
length(what_is_this)


class(desc.numeric(what_is_this[[1]],name = attributes(what_is_this)$names[1]))
elevatorFunction(what_is_this)[[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]]
attributes(elevatorFunction(what_is_this))
elevatorFunction<-function(z){
  # .vector <- NULL
   .vector_of_names <- attributes(z)$names
  for (i in 1:length(z)) {
    
   # .vector <- rbind(.vector,
    print(desc.numeric(what_is_this[[i]],name = .vector_of_names[i]))
    # ) 
  }
  # class(.vector)<-c(class(.vector),"agustin")
  # return(.vector)
}
elevatorFunction(what_is_this)
# Let's  do exactly the same thing but with datasets::mtcars
# Do not store it, just print it, just call the function

# That is what Fernando meant by "factors that are not defined as such in the database"

# You just do factor

# There you go, you are a genius

# Now, can we make a function taht generalizes the processing of this database and another one like datasets::mtcars, for example

# What is the difference?

# Well, in the datasets::mtcars, you must factor them yourself, therefore

# Could I use the function I coded today for the 9 types of elevators  I have?

# I don't think so



# I know why we are getting this complicated structure: you are creating a list on each iteration

# let's try 'rbind'

# What if you just fo a vector inside another vector and so on
# The output is really ugly with vectors



# If one of them is not normal (our case), you go with Levene's
# If they are all normal, you go with Bartlett's

x <- DATA$DV_SPEED_FPM
y <- DATA$`Device Type`


# But... Is this really telling me which one is better?



thisIsTheCoolestFunctionIHaveEverDoneInMyLifeTime(x,y,DEBUG=TRUE,digits=5)
# Can we do a graph?
library(ggplot2)
library(dplyr)
library(hrbrthemes)
install.packages("hrbrthemes")

ggplot(DATA,aes(x=DV_SPEED_FPM,fill=`Device Type`,colour=`Device Type`))+
  geom_density(alpha=0.5)+
  coord_cartesian(c(1,400))
?scale_x_continuous

# The sidewalk seems to be the one with more speed

# Speed is not the same as

ggplot(DATA,aes(x=DV_SPEED_FPM,fill=`Device Type`,colour=`Device Type`))+
  geom_boxplot(alpha=0.8)+
  coord_cartesian(c(1,400)) 


# Ok, I just did a graph
# And the passenger elevator is the most powerful one (in terms of speed)
# Doing the boxplot I have realized the Passenger Elevator is the most powerful one
# in terms of speed

# How do you make your graphs more readable?
attributes(datasets::mtcars)$names

# desc.numeric
# split()
# DATA$`Device Type`
# DATA$DV_SPEED_FPM
# attributes(datasets::mtcars$mpg$levels)
# Can we make our function split the data on more than one level?

# I mean, make it be able to process 9 types of elevator and say which one is faster?

# Recursion will save our lives here
# That is magnificent.. V-shaped makes less mpg than Straight therefore consuming more
# I also want my function to do graphs
# Shall we do DATA$vs <- factor(DATA$vs) # What are you doing with this?
y = DATA$vs






DATA_0
DATA_0<-getMe.nas(DATA_0)
x <- DATA_0$mpg
y <- DATA_0$vs

DATA_0 <- datasets::mtcars
DATA_0$vs <- factor(DATA_0$vs,levels = c(0,1),labels = c("V-shaped","Straight"))


# do NOT get confident
# Instead, keep working
# Trust me
# Why you not happy?
# What is there to be happy about?
# You just went 1-0
# Is job finished?
# Job finished?
# I don't think so



compare.samples


rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/myOwnFunctions.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/desc.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.nas.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/compare.independent.samples.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/freq_expected.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.summary.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.expectedVector.R", encoding = 'UTF-8')
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/desc.numeric.R", encoding = 'UTF-8')





if(TRUE){
rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.desc.factor.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.desc.numeric.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.just_to_increase_readibility.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.we_see_if_bartlett_or_wilcox.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/all_dot_in_dot_one.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/aux_function.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/compare.independent.samples.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/compare.samples.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/desc.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/freq_expected.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/frequencies.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.expectedVector.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.nas.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.summary.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/making.a.function.to.be.probabilistic.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/mode.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/print.agustin.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/prob_disc.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/test.for.normality.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/thisIsTheCoolestFunctionIHaveEverDoneInMyLifeTime.R")
}
if(TRUE){
  rm(list=ls())
}


# You are a genius 😯😯



frequencies




DATA<-datasets::mtcars
DATA<-getMe.nas(DATA)
DATA$vs <- factor(DATA$vs,levels = c(0,1),labels = c("V-shaped","Straight"))
DATA$am <- factor(DATA$am,levels = c(0,1),labels = c("Automatic","Manual"))
thisIsTheCoolestFunctionIHaveEverDoneInMyLifeTime(DATA$mpg,DATA$vs,DEBUG=TRUE,digits=10)

compare.samples(DATA$mpg,DATA$vs,DEBUG = TRUE,digits = 10)


compare.independent.samples(DATA$mpg,DATA$vs,DEBUG = DEBUG)
compare.independent.samples


if(TRUE){
rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.desc.factor.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.desc.numeric.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.just_to_increase_readibility.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.we_see_if_bartlett_or_wilcox.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/all_dot_in_dot_one.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/aux_function.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/compare.samples.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/desc.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/freq_expected.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/frequencies.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.expectedVector.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.nas.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.summary.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/making.a.function.to.be.probabilistic.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/mode.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/print.agustin.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/prob_disc.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/test.for.normality.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/thisIsTheCoolestFunctionIHaveEverDoneInMyLifeTime.R")
}





DATA<-getMe.nasRemoved(DATA)
library(ggplot2)
ggplot(DATA,aes(x=mpg,fill=vs,colour=vs))+
  geom_density(alpha=0.5)

ggplot(DATA,aes(x=mpg,fill=vs,colour=vs))+
  geom_boxplot(alpha=0.8)


# Let's just keep doing exercises

# I am thinking on practicing Data Bases on the afternoon's 
# It is not a bad idea, for sure

DATA$vs_am <- !is.na(DATA$am) & !is.na(DATA$vs) & DATA$vs==attributes(DATA$vs)$levels[1] & DATA$am==attributes(DATA$am)$levels[1]
  
  attributes(DATA$am)$levels[1]

  
  DATA$vs_am_factor <- factor(DATA$vs_am,levels = c(FALSE,TRUE),labels = c("Rest","Vs and Am"))

  
thisIsTheCoolestFunctionIHaveEverDoneInMyLifeTime(DATA$mpg,DATA$vs_am_factor,digits=20)  

# The rest of the cars make more miles per gallon than the V-shaped Automatic ones
# We have got that the V-shaped Automatic cars consume more than the rest

compare.samples(DATA$mpg,DATA$vs_am_factor,digits=20)
compare.samples
if(TRUE){
rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.desc.factor.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.desc.numeric.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.just_to_increase_readibility.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.we_see_if_bartlett_or_wilcox.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/all_dot_in_dot_one.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/aux_function.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/compare.samples.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/desc.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/freq_expected.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/frequencies.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.expectedVector.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.nas.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.summary.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/making.a.function.to.be.probabilistic.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/mode.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/print.agustin.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/prob_disc.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/test.for.normality.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/thisIsTheCoolestFunctionIHaveEverDoneInMyLifeTime.R")
}

.desc.numeric


.desc.numeric(DATA$cyl,name="cyl")


sum(DATA$cyl<=4&!is.na(DATA$cyl))


DATA$less_var <- DATA$cyl<=4 & !is.na(DATA$cyl)


DATA$less_var <- factor(DATA$less_var,levels = c(FALSE,TRUE),labels = c("Rest","Less carb"))

compare.samples(DATA$mpg,DATA$less_var,DEBUG=TRUE,digits=Inf)
thisIsTheCoolestFunctionIHaveEverDoneInMyLifeTime(DATA$mpg,DATA$less_var,DEBUG=TRUE,digits = Inf)


# Those with less than 4 carburetors do more miles per gallon than the rest
# Therefore, those with less than 4 carburetors 
# consume less than the rest


DATA$more_carb <- DATA$carb>=4 & !is.na(DATA$carb) 
DATA$more_carb <- factor(DATA$more_carb,levels = c(FALSE,TRUE),labels = c("Rest","More Carb"))

compare.samples(DATA$mpg,DATA$more_carb,DEBUG=TRUE,digits=Inf)
thisIsTheCoolestFunctionIHaveEverDoneInMyLifeTime(DATA$mpg,DATA$more_carb,DEBUG=TRUE,digits=Inf)


# This means that the Rest of cars (not those with 4 or more carburetors) do more miles per gallon
# Therefore, they consume less
mpg
x<-DATA
var1 <- "mpg"
var2 <- "more_carb"
alpha <- 0.8
graph.density<-function(x,var1,var2,alpha=0.05){

library(ggplot2)
ggplot(x,aes(x=var1,fill=var2,colour=var2))+
  geom_density(alpha=alpha)
}

graph.boxplot<-function(x,var1,var2,alpha=0.05){
  library(ggplot2)
  ggplot(x,aes(x=var1,fill=var2,colour=var2))+
    geom_boxplot(alpha=alpha)
}
library(ggplot2)

ggplot(aes(x=DATA$mpg,fill=DATA$more_carb,colour=DATA$more_carb))+
  geom_boxplot(alpha=0.05)

# Shall we do a function that automate doing the graph? Of course
graph.boxplot(x,var1,var2,alpha)
graph.density(x,var1=var1,var2=var2,alpha=alpha)


# mpg is a tibble, let's try with strings

# Let's just keep going with the document

# I really wanted to automate

# Well, we can do a snippet, you are right
# Yes, let's do snippets

library(ggplot2)

ggplot(DATA,aes(x=mpg,fill=more_carb,colour=more_carb))+
geom_boxplot(alpha=0.05)

library(ggplot2)

ggplot(DATA,aes(x=mpg,fill=more_carb,colour=more_carb))+
geom_density(alpha=0.05)


# Snippets are really cool



# Let's go for the rest of exercises and then we clean the bathroom and then we workout and tomorrow we keep up the good work

# I am going to crush my upcoming exams and then I am going to be on 3rd year by my 20-year-old birthday

# I am going to study Computer Science on September and I am going to be on the residency

# Let's see if they have put the date already


(1320+660)*(1320+3960)/(0+3960+660+1320)



freq_expected
frequencies


.desc.factor
freq_expected
# I did a function that gets me a data.frame

# I do not remember its name


# I found it: it's called "getMe.summary"

# As one article said: "The hardest part is knowing how to name your functions"


if(TRUE){
rm(list=ls())
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.desc.factor.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.desc.numeric.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.just_to_increase_readibility.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/.we_see_if_bartlett_or_wilcox.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/all_dot_in_dot_one.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/aux_function.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/compare.samples.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/desc.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/freq_expected.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/frequencies.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.expectedVector.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.nas.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/getMe.summary.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/making.a.function.to.be.probabilistic.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/mode.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/print.agustin.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/prob_disc.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/test.for.normality.R")
source("C:/Users/Agus/OneDrive - Universidad de Castilla-La Mancha/Escritorio/2do año/2do cuatri/Statistics (Fernando)/R/Libraries/thisIsTheCoolestFunctionIHaveEverDoneInMyLifeTime.R")
}


DATA<-datasets::mtcars
DATA<-getMe.nas(datasets::mtcars)


DATA$vs <- factor(DATA$vs,levels = c(0,1),labels = c("V-shaped","Straight"))
DATA$am <- factor(DATA$am,levels = c(0,1),labels = c("Automatic","Manual"))


getMe.summary()


getMe.summary(DATA$vs,DATA$am)
getMe.summary(DATA$am,DATA$vs)


# Something that would be really cool would be to do a data.frame and in each position we have 3 values, it is like having a list really

# How can you do that
# That would be so easy to see
# Not so easy to code though
# Nevertheless, it is not impossible




knitr::kable(data.frame(
  vs = c("V-shaped","Straight"),
  Automatic = c(list(observed=10,prop = 63.33,exp=2.4555),c(observed=7,prop=33.33,exp=5.3677)),
  Manual = c(list(observed=4,prop=37.89,exp=10.455),c(observed=14,prop=63.11,exp=6.78))
),
format="simple")


# That is even better --> to have a data.frame for the obs, prop and exp

t_freq <- table(DATA$vs,DATA$am)
t_prop <- prop.table(t_freq)
t_prop <- round(t_prop*100,digits = 2)
t_prop
class(t_prop)<-c(class(t_prop),"agustin")
class(t_freq)<-c(class(t_freq),"agustin")
print(t_prop)


# Now, we are just missing our Expected table

m <- t_freq

getMe.expectedVector(t_freq[1])
letsTryThisBaby<-function(m){

.my_df <- m

for (j in attributes(m)$dimnames[[2]]) {
  for (i in attributes(m)$dimnames[[1]]) {
    .my_df[i,j] <- freq_expected(m,i,j)
  }
}
class(.my_df)<-c(class(.my_df),"agustin")
.my_df
}

tolower(attributes(m)$dimnames[[2]][2])
getMe.expectedVector


freq_expected(t_freq[attributes(t_freq)$dimnames[[2]][2],attributes(t_freq)$dimnames[[1]][1]])
t_freq
letsTryThisBaby(t_freq)


# I have done it: again 😎
# This is brilliant and most of all: SIMPLE
# I can't believe you are correct
# This is magic

cbind(data.frame(agustin=20,ainara=21),guzman = vector)

vector <- c(44)
names(vector) <- "guzman"
freq_expected

x <- DATA$vs
y <- DATA$am



letsSeeThisOutputRightHere(DATA$vs,DATA$am)
# It seems that you can't do that

# Let's just print the three tables

attributes(table(x,y))






IWantThe(x,y,"t")
IWantThe(x,y,"d")


# Amazing job, Agustin

# Tomorrow we finish the proportions function and get the workflow really interiorized for the exam day

# We are going to be on 3rd year by my 20-year-old birthday



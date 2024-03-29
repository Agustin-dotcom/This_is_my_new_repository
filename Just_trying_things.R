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





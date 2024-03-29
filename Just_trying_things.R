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
thisIsTheCoolestFunctionIHaveEverDoneInMyLifeTime<-function(x,y,DEBUG = FALSE){
# First, we must check normality

# As previously, we are going to assume 'y' is already factored

# We are going to call desc.numeric if you want

all_of_them_are_normal <- TRUE

n_samples <- split(x,y)
class(split(x,y)[[1]])
for (i in 1:length(n_samples)) {
    if (!desc.numeric(n_samples[[1]])$is.normal) {
      all_of_them_are_normal <- FALSE
    }
}
# I just want to know if all of them are nor or not
# To know wheter we should go with Bartlett's or Levene's
# And I do not have to worry about using Lillie's or Shapiro since
# desc.numeric is already smart enough (at least, that is what I remember)
# let's check
# Yes, it is. By the way, desc.numeric excludes NAs always
# Why would you want NAs anyways?


desc.numeric(n_samples[[1]])
# It works 😁😁
# Let's have some lunch and then 
# Keep doing this amazing function


# desc.numeric
if(all_of_them_are_normal){
  if (DEBUG) print(paste("I do bartlett's."))
    .p.value<-bartlett.test(x ~ y)$p.value
  .result <- .just_to_increase_readibility(x,y,.p.value)
  class(.result)<-c(class(.result),"agustin")
  return(.result)
}else
{
  #As soon as you know one of your samples is not normal, you do Wilcox
  if(DEBUG) print(paste("I do Levene's."))
  ?wilcox.test
  # We found a limitation here
  # Unless we recursion on comparing and find the best one
  .p.value <- wilcox.test(n_samples[[1]],n_samples[[2]])
  .result<-.just_to_increase_readibility(x,y,.p.value)
  class(.result)<-c(class(.result),"agustin")
  return(.result)
}

}

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
  geom_boxplot(alpha=0.05)+
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
compare.samples<-function(x,y,DEBUG = FALSE,na.rm = TRUE){
  
  
  first_f <- attributes(y)$levels[1]
  second_f <- attributes(y)$levels[2]
  ?tapply
  
  first_sample <- split(x,y)[[1]]
  second_sample <- split(x,y)[[2]]
  if(na.rm)
  {
    first_sample <- first_sample[!is.na(first_sample)]
    second_sample <- second_sample[!is.na(second_sample)]
  }
  first_l <- length(first_sample)
  second_l <- length(second_sample)
  
  if(first_l<3 | second_l < 3) stop("One of your samples (when dividing) does not have more than 3 samples!! 😬")
  else
  {
    if(first_l >5000 | second_l>5000)
    {
      if(first_l>5000 & second_l > 5000){
        test <- tapply(x,y,nortest::lillie.test)
        
        
        first_p <- test[[1]]$p.value
        second_p <- test[[2]]$p.value
        .result <- .we_see_if_bartlett_or_levene(x,y,first_p_value = first_p,second_p_value = second_p,DEBUG = DEBUG)
        class(.result)<-c(class(.result),"agustin")
        return(.result)
      }else
      {
        if(first_l>5000){
          if(DEBUG) print(paste("We split and we use Lillie in the first one and Shapiro in the second one"))
          first_p<-nortest::lillie.test(first_sample)$p.value
          second_p<-shapiro.test(second_sample)$p.value
          .result <- .we_see_if_bartlett_or_levene(x,y,first_p_value = first_p,second_p_value = second_p,DEBUG=DEBUG)
          class(.result)<-c(class(.result),"agustin")
          return(.result)
        }else
        {
          first_p<-shapiro.test(first_sample)$p.value
          second_p<-nortest::lillie.test(second_sample)$p.value
          .result <- .we_see_if_bartlett_or_levene(x,y,first_p_value = first_p,second_p_value = second_p,DEBUG=DEBUG)
          class(.result)<-c(class(.result),"agustin")
          return(.result)
        }
      }
    }else
    {
      # Just do the regular thing
      test <- tapply(x, y, shapiro.test)
      first_p <- test[[1]]$p.value
      second_p <- test[[2]]$p.value
      
      
      # There is no problem to do shapiro.test with NAs
      # Maybe it is good to try it with the Elevators Data Base
      # We also have to take into account NAs
      # We also must return the p value and what we used
      # Otherwise, we won't have information
      # compare.independent.samples
      .result <- .we_see_if_bartlett_or_levene(x,y,first_p_value = first_p,second_p_value = second_p,DEBUG=DEBUG)
      class(.result)<-c(class(.result),"agustin")
      return(.result)
    }
  }
}



.we_see_if_bartlett_or_levene<-function(x,y,first_p_value,second_p_value,DEBUG = FALSE){
  if(!(first_p_value<0.05)&!(second_p_value<0.05)){
    if(DEBUG) print(paste("Using Bartlett's (both are normal)."))
    b_test <- bartlett.test(x ~ y)
    .p.value <- b_test$p.value
    return(.just_to_increase_readibility(x,y,p.value = .p.value,DEBUG=DEBUG))
  }else
  {
    if(DEBUG) print(paste("At this point, we know one of them is not normal, so we can't use bartlett's but Levene's"))
    .p.value<-car::leveneTest(x ~ y )[1,3]
    return(.just_to_increase_readibility(x,y,p.value=.p.value,DEBUG=DEBUG))
  }
}


.just_to_increase_readibility<-function(x,y,p.value,DEBUG=FALSE){
  if(p.value<0.05){
    .p.value <- t.test(x ~ y,var.equal = F)$p.value
    if(.p.value<0.05){
      if(DEBUG) print(paste("We reject the null hypothesis"))
      if (DEBUG) print(paste("THERE is a significant statistical difference"))
      if(DEBUG) print(paste("Which one is greater?"))
      .p.value <- t.test(x ~ y, var.equal = F,alternative = "less")$p.value
      if(DEBUG){
        print(paste("H0: x[x$y_1] >= x[x$y_2]"))
        print(paste("H1: x[x$y_1] < x[x$y_2]"))
      }
      if(DEBUG) print(paste0(".p.values is ",.p.value))
      if(.p.value<0.05){
        if(DEBUG) print(paste("we reject the null hypothesis"))
        print(paste0(first_f," is less than ",second_f))
      }else
      {
        if(DEBUG) print(paste("We fail to reject the null hypothesis."))
        print(paste0(first_f," is greater or equal than ",second_f))
      }
    }else
    {
      if(DEBUG) print(paste("We fail to reject the null hypothesis."))
      if(DEBUG) print(paste("We say there is NO significant statistical difference."))
      print(paste0("There is no statistical difference between the two samples"))
    }
  }else
  {
    .p.value <- t.test(x ~ y,var.equal = T)$p.value
    if(DEBUG) print(paste0(".p.value is ",.p.value))
    if(.p.value<0.05){
      if(DEBUG){ 
        print(paste("We reject the null hypothesis."))
        print(paste("Therefore, there is a significant statistical difference between these two."))
        print(paste("Let's see which one is greater."))
       }
      .p.value <- t.test(x ~ y, var.equal = T,alternative = "greater")$p.value
      if(DEBUG){
        print(paste("H0: x[x$y_1] <= x[x$y_2]"))
        print(paste("H1: x[x$y_1] > x[x$y_2]"))
      }
      if(DEBUG) print(paste0(".p.value is ",.p.value))
      if(.p.value<0.05){
        
        if(DEBUG) print(paste("We reject the null hypothesis."))
        print(paste0(first_f," is greater than ",second_f))
      }else
      {
        
        if(DEBUG) print(paste("We fail to reject the null hypothesis."))
        print(paste0(first_f," is less or equal ",second_f))
      }
    }else
    {
      if(DEBUG){
        print(paste("We fail to reject the null hypothesis."))
        print(paste("There is not a significant statistical difference."))
      }
      print(paste0("Both samples are equal: there is no significant statistical difference."))
    }
  }
  return(.p.value)
}

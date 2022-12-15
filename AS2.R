babies <- list(
  name = c("Caroline", "John", "Lisa", "Daniel"),
  birht = c("natural", "c-section", "c-section", "natural", "c-section"),
  formula = c("yes", "no", "no", "no")
)

length(babies)                          #a
lapply(babies, length)


names(babies)[2] <- "birth"             #b
babies

babies$birth <- babies$birth[-5]
babies


sex <- factor(c("f", "m", "f", "m"))
sex
  
babies[["sex"]] <- sex                 #c
babies

babies$formula <- NULL                 #d
babies

Caroline <- list(                      
  exam = c("U1", "U2", "U3"),
  weight = c(3276, 3921, 4601),
  formula = c("yes", "no", "no"))

John <- list(
  exam = c("U1", "U2", "U3"),
  weight = c(3745, 4592, 5012),
  formula = c("no", "no", "no"))
  
Lisa <- list(
  exam = c("U1", "U2", "U3"),
  weight = c(2898, 3205, 3982),
  formula = c("yes", "yes", "yes"))
  
Daniel <- list(
  exam = c("U1", "U2", "U3"),
  weight = c(3300, 3799, 4235),
  formula = c("no", "no", "yes"))

babies_exams <- list(babies, Caroline, John, Lisa, Daniel)

babies_exam <- babies
babies_exam$Caroline <- Caroline
babies_exam$John <- John
babies_exam$Lisa <- Lisa
babies_exam$Daniel <- Daniel
babies_exam


names(babies_exam)                      #e
lapply(babies_exam, names)

weight <- list(Caroline$weight, John$weight, Lisa$weight, Daniel$weight)    #f
names(weight) <- c("Caroline", "John", "Lisa", "Daniel")
sapply(weight, mean)


df_babies <- data.frame(babies)        #g
df_babies

typeof(df_babies)    # type is list
class(df_babies)     # class is data frame
dim(df_babies)       # dimension is 4 3


mean_weight <- sapply(weight, mean)               #h
df_babies[["mean_weight"]] <- mean_weight
df_babies

x <- which(df_babies$mean_weight > 4000, df_babies$sex == "m")   #i
print(df_babies[x,])

 

exams <- data.frame(Caroline)             #j
exams <- rbind(exams, John)
exams <- rbind(exams, Lisa)
exams <- rbind(exams, Daniel)


index <- rep(c(1,2,3,4), each = 3)    

df_index <- data.frame(index)
index
exams <- cbind(df_index, exams)  
exams

name <- rep(c("Caroline", "John", "Lisa", "Daniel"), each = 3)     #k
df_name <- data.frame(name)
exams <- cbind(df_name, exams)
exams[["index"]] <- NULL
exams




# - -----------------------------------------------------------------------

# Roll dice function
#
# This function simulates rolling a dice with 6 sides.
# Arguments:
# num:       An integer. It defines the number of dice rolls to be simulated.
#
# prob:      A dobble vector. It defines a vector of six probabilities for each side of the dice.
#            All elements of this vector should add up to one.
# ergebnis:  An integer vector. The result of the function roll dice.



roll_dice <- function(num, prob) {                                #a
 ergebnis <- sample(1:6, num, replace = TRUE, prob = prob)
 return(ergebnis)
}
roll_dice(15, c(0.1, 0.2, 0.2, 0.02, 0.08, 0.4))


# Roll dice function
#
# This function simulates rolling a dice with 6 sides.
# Arguments:
# num:       An integer. It defines the number of dice rolls to be simulated.
#            Can't be smaller than 1.
#
# prob:      A dobble vector. It defines a vector of six probabilities for each side of the dice.
#            All elements of this vector should add up to one. The length of the vector
#            must be 6.

# ergebnis:  An integer vector. The result of the function roll dice.



roll_dice <- function(num, prob) {                               #b
    if(num < 1 | typeof(num) != "integer") {
      return("num passt nicht")
  } 
    else if(length(prob) != 6 | sum(prob) != 1){
      return("length passt nicht")
  } 
    else {
      ergebnis <- sample(1:6, num, replace = TRUE, prob = prob)
      return(ergebnis)
  }
  
}


roll_dice(15L, c(0.1, 0.4, 0.4, 0.08, 0, 0.02))
  


# Summarize dice function
#
# This function returns the number of times each side of the dice was realized.
# Arguments:
# num:       An integer. It defines the number of dice rolls to be simulated.
#            Can't be smaller than 1.
#
# prob:      A dobble vector. It defines a vector of six probabilities for each side of the dice.
#            All elements of this vector should add up to one. The length of the vector
#            must be 6.

# return:    An integer. Returns the result of the function summarize dice. 


summarize_dice <- function(num, prob) {                #c
  x <- roll_dice(num, prob)
  probies <- table(x)
  return(probies)
} 

summarize_dice(15L, rep(1/6, 6))



# List rolls function
#
# This function returns the number of times each side of the dice was realized.
# Arguments:
# num:       An integer. It defines the number of dice rolls to be simulated.
#            Can't be smaller than 1.
#
# prob:      A dobble vector. It defines a vector of six probabilities for each side of the dice.
#            All elements of this vector should add up to one. The length of the vector
#            must be 6.

# return:    A list. It returns the results of function list rolls. Specifically:
#            the input probability prob rounded to 2 decimal places,
#            the relative frequency of each of the six sides of the dice as realized
#            by function roll dice with inputs num and prob, rounded to 2 decimal places,             
#            a boolean vector of length 6 that contains TRUE if the respective element
#            of relative freq and input prob are equal and a boolean vector of length 1. TRUE if all elements of equal are
#            TRUE, FALSE otherwise.


list_rolls <- function(num, prob) {                #d
  
  input_prob <- round(prob, digits = 2)
  biba <- summarize_dice(num, prob)
  relative_freq <- round((biba / sum(biba)), 2)
  equal <- input_prob == relative_freq
  all_equal <- all(equal == TRUE)
  u <- list(input_prob, relative_freq, equal, all_equal)
  names(u) <- c("input_prob", "relative_freq", "equal", "all_equal")
  return(u)
  
}






library(ggplot2)

set.seed(203)

# Problem 1: Create data for these four variables for 30 households. 
income = rnorm(30, mean = 100, sd =200)
access_to_educ = rnorm(30, mean = 2.5, sd =2)
health_care = rnorm(30,mean = 300, sd =100)
num_child = round(rnorm(30,mean = 4, sd =1.2))

# Problem 2: Create an error variable where n = 30, 
# with a mean of 0 and standard deviation of 75. 
# Also define an ‘intercept’ variable that is equal to 150.
error = rnorm(30,mean = 0,sd =75)
intercept = 150


# Problem 3: Define our education spending variable using the following formula:
educ_spending = intercept + 0.04*income + 40*access_to_educ +
  (-0.25)*health_care + 50*num_child + error

df = data.frame(educ_spending, income,access_to_educ,
                health_care,num_child)

# Problem 4: Create a scatter plot with access to education on the x axis 
# and education spending on the y axis. 
ggplot(df, aes(x=access_to_educ, y=educ_spending)) + geom_point()



ggplot(df, aes(x=health_care, y=educ_spending)) + geom_point()



# Problem 5: Run a linear model on the dataframe, regressing education 
# spending on the 4 independent variables.
model_n30 =lm(formula = 'educ_spending ~ income + access_to_educ + health_care + num_child', data = df)
summary(model_n30)




# Problem 6: Create a function called ‘data_generating_process’ that takes
# in a value for n and outputs a dataframe with education spending, income, 
# access to education, health care costs, and number of children. 
data_generating_process = function(n){
  income = rnorm(n, mean = 100, sd = 200)
  access_to_educ = rnorm(n, mean = 2.5, sd = 2)
  health_care = rnorm(n,mean = 300, sd = 100)
  num_child = round(rnorm(n, mean = 4, sd = 1.2))
  
  error = rnorm(n,mean = 0,sd =75)
  intercept = 150
  
  educ_spending = intercept + 0.04*income + 40*access_to_educ +
    (-0.25)*health_care + 50*num_child + error
  
  df = data.frame(educ_spending, income,access_to_educ,
                  health_care,num_child)
  return(df)
}

# N = 300
df_300 = data_generating_process(300)
# N = 3000
df_3000 = data_generating_process(3000)

# Problem 7: Linear regression models for both n
model_n300 =lm(formula = 'educ_spending ~ income + access_to_educ + health_care + num_child', data = df_300)
model_n3000 =lm(formula = 'educ_spending ~ income + access_to_educ + health_care + num_child', data = df_3000)


# Problem 8: What would happen to the outputs of the linear model if you
# increase the standard deviation of the independent variables and the error? 
data_generating_process_sd = function(sd_scalar){
  age = rnorm(30,mean = 100, sd =200*sd_scalar)
  class = rnorm(30, mean = 2.5, sd =2*sd_scalar)
  parents_educ = rnorm(30,mean = 300, sd =100*sd_scalar)
  parents_occupation = round(rnorm(30, mean = 4, sd =1.2*sd_scalar))
  error = rnorm(30,mean = 0,sd = 75*sd_scalar)
  intercept = 150
  
  enrolment = intercept + 0.04*age + 40*class +
    (-0.25)*parents_educ + 50*parents_occupation + error
  
  df = data.frame(enrolment, age,class,
                  parents_educ,parents_occupation)
  
  model = lm(formula = 'enrolment ~ age + class + parents_educ + parents_occupation', data = df)
}

model_n30_sd2 = data_generating_process_sd(2)
# After checking the function and going through the problems 1-7, I notice that
# when I increase the standard deviation of the independent variables, 
# the variable coefficients increase, resulting in an increase on the slope.
# Additionally, increasing the error will also affect the output by also 
# increasing the education spending. This increase in error will factor in any
# other correlations that the model hasn't factored in.

print(summary(model_n30))
print(summary(model_n300))
print(summary(model_n3000))
print(summary(model_n30_sd2))

# import data
rm(list=ls())
df=read.csv("D:/Macquarie/Term 2-2024/ACST8086 Actuarial Modelling/Assignment_data.csv")
View(df)

##############Preliminary data analysis##############

##Data cleaning

#Changing column names
colnames(df)[2]="Training.Data"

#Tuning Data type
df["Training.Data"] = as.numeric(unlist(df["Training.Data"]))

#Dealing with Na values
na_val=sum(is.na(df))
print(na_val)
df["Training.Data"][is.na(df["Training.Data"])] = mean(df[["Training.Data"]], na.rm = TRUE)

#EDA analysis 
summary(df)

#Dealing with near-0-values
df["Training.Data"]=abs(df["Training.Data"])
df["Validation.Data"]=abs(df["Validation.Data"])
df["Test.Data"]=abs(df["Test.Data"])

#EDA analysis 
summary(df)

##Scatter Plot grapphing
#Install relative libraries
#install.packages("ggplot2") 
library(ggplot2)
library(gridExtra)

#Plot Histogram for Training.Dataset
training_plot= ggplot(df, aes(x = Area.ID, y = Training.Data)) +
  geom_point(color = "blue", size = 3) +  # Points color and size
  ggtitle("Scatterplot for Training.Data") + # Title
  xlab("Area.ID") +                  # X-axis label
  ylab("Training.Data") +                  # Y-axis label
  theme_minimal()                         # Minimal theme 

#Plot Histogram for Validation dataset
validation_plot= ggplot(df, aes(x = Area.ID, y = Validation.Data)) +
  geom_point(color = "red", size = 3) +  # Points color and size
  ggtitle("Scatterplot for Validation.Data") + # Title
  xlab("Area.ID") +                  # X-axis label
  ylab("Validation.Data") +                  # Y-axis label
  theme_minimal()                         # Minimal theme 

#Plot Histogram for Test dataset
test_plot= ggplot(df, aes(x = Area.ID, y = Test.Data)) +
  geom_point(color = "green", size = 3) +  # Points color and size
  ggtitle("Scatterplot for Test.Data") + # Title
  xlab("Area.ID") +                  # X-axis label
  ylab("Test.Data") +                  # Y-axis label
  theme_minimal()                         # Minimal theme 

# Arrange plots in a 2x2 grid
grid.arrange(training_plot, validation_plot, test_plot,ncol = 2)

##Box Plot Graphing
boxplot(df["Training.Data"], main = "Boxplot For Training.Data", xlab = "Average Premium ", ylab = "Training Data", horizontal = T, col="red", ylim=c(0,10000))
boxplot(df["Validation.Data"], main = "Boxplot For Validation.Data", xlab = "Average Premium ", ylab = "Validation Data", horizontal = T, col="yellow", ylim=c(0,10000))
boxplot(df["Test.Data"], main = "Boxplot For Test.Data", xlab = "Average Premium ", ylab = "Test Data", horizontal = T, col="green", ylim=c(0,10000))

par(mfrow=c(2,2))
##############Parametric curve fitting##############

##############Model 1: Parametric Regression##############

##Generlized linear regression
linear_model=glm(Training.Data~Area.ID, data=df,family= gaussian)

#fitting data
linear_predict=predict(linear_model,df["Area.ID"])
print(linear_model)

##Compute MSE
linear_score=(df["Training.Data"]-linear_predict)**2
mse_score=mean(unlist(linear_score))
print(mse_score)

##Polynomial model (degree of freedom from 2 to 7)
for (i in 2:8){
  #creating model
  model=lm(Training.Data~poly(Area.ID, i), data=df)
  
  #fitting data
  predict=predict(model,df["Area.ID"])
  
  ##Compute MSE
  score=(df["Training.Data"]-predict)**2
  mse_change=mse_score-mean(unlist(score))
  cat("with 1 increase in degree of freedom, MSE decreases by:",mse_change,"\n")
  mse_score=mean(unlist(score))
  cat("polinimonal model with degree of freedom of ",i," has an MSE of ",mse_score,"\n")
  cat("\n")
}
#conclude: the higher the degree, the lower the MSE
#Choose function where 1 increase in degree has the highest decrease in MSE: degree of 4

#fitting model
model1=lm(Training.Data~poly(Area.ID, 4), data=df)
model1_predict=predict(model1,df["Area.ID"])
print(model1)

# Plot the original points and the regression fit
plot(x=df[["Area.ID"]], y=df[["Training.Data"]], type = "p", main = "Generalized linear model", col = "blue", pch = 19, xlab = "X", ylab = "Y")
lines(x=df[["Area.ID"]], y=unlist(model1_predict), col = "red", lwd = 2)


##############Model 2: Natural Cubic Spline##############

#Function to calculate Chebyshev nodes
chebyshev_nodes <- function(n) {
  k = 1:n
  nodes <- cos((2 * k - 1) * pi / (2 * n))  #Chebyshev spacing function, refering to the report
  return(nodes)
}

# Function to calculate Chebyshev nodes in the column
chebyshev_nodes_scaled <- function(n, a) {
  nodes <- chebyshev_nodes(n)
  #Scale Chebyshev nodes to real data size
  scaled_nodes <- (max(df[a]) - min(df[a])) / 2 * nodes + (max(df[a]) + min(df[a])) / 2 
  return(scaled_nodes)
}

# Calculate 7 Chebyshev nodes
rv_nodes = chebyshev_nodes_scaled(7,"Area.ID")
nodes= rv_nodes[order(rv_nodes, decreasing = FALSE)]
print(nodes)

#define knot case matrix
knot_comb=list()
for (i in 0:7) {
  knot_case=combn(nodes,i)
  knot_comb=append(knot_comb,list(knot_case))
}

#create lists for knot cases and its MSE
knot_case_filtered=c()
test_score_filtered=c()

#calculate goodness of fit for each knot case
#install.packages("splines")
library(splines)
for (i in 2:8){
  #calculate number of combinations with i participles
  k=(length(knot_comb[[i]])/(i-1))
  for (j in 1:k){
    fit_spline = lm(df[["Training.Data"]] ~ ns(df[["Area.ID"]], knots = knot_comb[[i]][,j]))
    
    # Make predictions using the fitted model
    predicted_training_data <- predict(fit_spline, df["Area.ID"])
    
    #calculate MSE
    score=(df["Validation.Data"]-predicted_training_data)**2
    mse=mean(unlist(score))
    knot_case_filtered=append(knot_case_filtered,list(knot_comb[[i]][,j]))
    test_score_filtered=append(test_score_filtered,mse)
  }
}
test_mapping=setNames(test_score_filtered,knot_case_filtered)
#print(test_mapping)
sorted_test_mapping= test_mapping[order(test_mapping, decreasing = FALSE)]
print(sorted_test_mapping)

# Conclude lowest MSE knot combination:
# with 6 nodes (1267345):  c(7.84467997436212, 60.5600052862279, 155.549739220907, 274, 487.439994713772, 540.155320025638)  
# with 5 nodes (1269359): c(60.5600052862279, 155.549739220907, 392.450260779093, 487.439994713772, 540.155320025638)  
# with 4 nodes (1277446):    c(60.5600052862279, 155.549739220907, 274, 540.155320025638)

# Make predictions using the fitted model
fit_cubic_spline = lm(df[["Training.Data"]] ~ ns(df[["Area.ID"]], knots = c(7.84467997436212, 60.5600052862279, 155.549739220907, 274, 487.439994713772, 540.155320025638)))
cubic_predict= predict(fit_cubic_spline, df["Area.ID"])

# Compute MSE
cubic_score=(df["Training.Data"]-cubic_predict)**2
cubic_test_score=mean(unlist(cubic_score))
print(cubic_test_score)

# Plot the original points and the natural spline fit
plot(x=df[["Area.ID"]], y=df[["Training.Data"]], type = "p", main = "Cubic Natural Spline Chart", col = "blue", pch = 19, xlab = "X", ylab = "Y")
lines(x=df[["Area.ID"]], y=cubic_predict, col = "red", lwd = 2)

##############Model 3: Smoothing Spline##############
# Range of spar values
spar_values = seq(0, 1.5, by = 0.01)

# Vector to store Sum of squared standardised deviations for each spar value
ssd_values = numeric(length(spar_values))

# Vector for MSE values
mse_values=c()

# Fit model for each spar value
for (i in seq_along(spar_values)) {
  spar= spar_values[i]
  
  # Fit a smoothing spline with the current spar value
  fit_spline= smooth.spline(x = df[["Area.ID"]], y = df[["Training.Data"]], spar = spar)
  
  # Predict on validation data
  predicted_validation= predict(fit_spline, df["Area.ID"])
  predicted_validation= predicted_validation["y"]
  
  # Calculate MSE on the validation data
  mse= (df["Validation.Data"]-predicted_validation)**2
  mse_values[i] = sum(unlist(mse))
  
  cat("Spar:", spar, "MSE:", mse_values[[i]], "\n")
}

# Find the spar value that gives the lowest MSE
best_spar <- spar_values[which.min(mse_values)] 
cat("Best spar value:", best_spar, "\n")
cat("Lowest MSE:", min(mse_values), "\n")
#conclude best lambda at number very close to 0

# Make predictions using the fitted model
fit_smooth_spline= smooth.spline(x = df[["Area.ID"]], y = df[["Training.Data"]], spar=0.01)
smooth_predict= predict(fit_smooth_spline, df["Area.ID"])
smooth_predict= smooth_predict["y"]

#Compute MSE
smooth_score=(df["Training.Data"]-smooth_predict)**2
smooth_test_score=mean(unlist(smooth_score))
print(smooth_test_score)

# Plot the original points and the smoothing spline fit
plot(x=df[["Area.ID"]], y=df[["Training.Data"]], type = "p", main = "Smooth Spline Chart", col = "blue", pch = 19, xlab = "X", ylab = "Y")
lines(x=df[["Area.ID"]], y=unlist(smooth_predict), col = "red", lwd = 2)

##compare Out-of-sample test
#for model 1
model1_mse_score=(df["Test.Data"]-model1_predict)**2
model1_mse_test_score=mean(unlist(model1_mse_score))
print(model1_mse_test_score)

#for model 2
cubic_mse_score=(df["Test.Data"]-cubic_predict)**2
cubic_mse_test_score=mean(unlist(cubic_mse_score))
print(cubic_mse_test_score)

#for model 3
smooth_mse_score=(df["Test.Data"]-smooth_predict)**2
smooth_mse_test_score=mean(unlist(smooth_mse_score))
print(smooth_mse_test_score)
#Conclude: choose smooth spline for its lowest MSE

##############Graduation Tests##############
#H0: smooth spline can predict average training data
#H1: smooth spline can not predict average training data (inaccurate prediction)
# 2-tailed test, critical region of 5%

##i. Chi-squared test of fit
z_score=unlist((df["Training.Data"]-smooth_predict)/lapply(smooth_predict, sqrt))
z_score_test=sum(z_score**2)
print(z_score_test)

#find optimal degree of freedom where fail to reject hypothesis
i=1
critical_value=0

while(i<=547 & z_score_test>critical_value){
  critical_value=qchisq(0.95,i)
  cat("degree of freedom:", i,"  critical value:",critical_value,"\n")
  i=i+1
}
#Conclusion: Reject hypothesis

##ii. Standardised deviations test
#create a vector of actual z score number of each interval
z_actual=c()
#create a vector of expected z score number of each interval
z_expected=c()

#Choose intervals and print actual z scores number within (8 intervals, as can be found below)
z_actual=append(z_actual,length(z_score[z_score< (-3)]))
z_actual=append(z_actual, length(z_score[(-3)<z_score & z_score<(-2)]))
z_actual=append(z_actual, length(z_score[(-2)<z_score & z_score<(-1)]))
z_actual=append(z_actual, length(z_score[(-1)<z_score & z_score<0]))

z_actual=append(z_actual, length(z_score[1>z_score & z_score>0]))
z_actual=append(z_actual, length(z_score[2>z_score & z_score>1]))
z_actual=append(z_actual, length(z_score[3>z_score & z_score>2]))
z_actual=append(z_actual, length(z_score[z_score>3]))

#calculate expected number of observations within each interval 
z_middle_expected=(pnorm(3,0,1, lower.tail = TRUE)-pnorm(-3,0,1, lower.tail = TRUE))*length(z_score)
z_expected=append(z_expected, (length(z_score)-z_middle_expected)/2)
z_expected=append(z_expected, (pnorm(-2,0,1, lower.tail = TRUE)-pnorm(-3,0,1, lower.tail = TRUE))*length(z_score))
z_expected=append(z_expected, (pnorm(-1,0,1, lower.tail = TRUE)-pnorm(-2,0,1, lower.tail = TRUE))*length(z_score))
z_expected=append(z_expected, (pnorm(0,0,1, lower.tail = TRUE)-pnorm(-1,0,1, lower.tail = TRUE))*length(z_score))

z_expected=append(z_expected, (pnorm(1,0,1, lower.tail = TRUE)-pnorm(0,0,1, lower.tail = TRUE))*length(z_score))
z_expected=append(z_expected, (pnorm(2,0,1, lower.tail = TRUE)-pnorm(1,0,1, lower.tail = TRUE))*length(z_score))
z_expected=append(z_expected, (pnorm(3,0,1, lower.tail = TRUE)-pnorm(2,0,1, lower.tail = TRUE))*length(z_score))
z_expected=append(z_expected, (length(z_score)-z_middle_expected)/2)

sq_error=(z_actual-z_expected)**2/z_expected
sd_score=sum(sq_error)
cat("test score: ",sd_score)
cat("critical value: ", qchisq(0.05, 7))
#Conclude: Reject null hypothesis

##iii. Signs test
#calculate probability of z=i
at_i_prob=c()
for (i in 1:length(unlist(smooth_predict))){
  case_count = choose(length(unlist(smooth_predict)), i)
  at_i_prob=append(at_i_prob, case_count/(2^length(unlist(smooth_predict))))
}

#calculate probability of z<=i
below_i_prob=0
j=1
while (below_i_prob<0.025){
  below_i_prob=below_i_prob+ at_i_prob[j]
  cat("Probability of z <=",j,": ",below_i_prob,"\n")
  j=j+1
}
#Conclusion: smallest k for P(z<=k)>=0.025 is 251
cat("lower_bound: ", j-1)
cat("upper_bound: ", length(unlist(smooth_predict))-j)
over_0_data=length(z_score[z_score>=0])
below_0_data=length(z_score[z_score<0])
cat("observed data from Standardised Deviations Test: ", over_0_data)
#Conclude: k<P<m-k, fail to reject the null hypothesis at a 5% significance level.

##iv. Cumulative deviations test
cd_score=(sum(df["Training.Data"])-sum(unlist(smooth_predict)))/(sum(unlist(smooth_predict))^0.5)
print(cd_score)
#Conclude: fail to reject (-1.96<cd_score<1.96)

##v. Grouping of signs test

#calculate probability of z<=i
below_i_prob_group=0
i=1
while (below_i_prob_group<0.05){
  case_count_positive = choose(over_0_data-1, (i-1))
  case_count_negative = choose(below_0_data+1, i)
  case_sign=choose(length(z_score),over_0_data)
  below_i_prob_group=below_i_prob_group+ case_count_negative*case_count_positive/case_sign
  cat("Probability of group sign z <=",i,": ",below_i_prob_group,"\n")
  i=i+1
}
#Conclusion: group sign test smallest k for P(z<=k)>=0.05 is 128

cat("expected number of data >0:",over_0_data)
#k=128<G=267: fail to reject the null hypothesis.


##vi. Serial correlations test for 1st lag
z_x=z_score[1:546]
z_x_plus=z_score[2:547]

#calculating serial coefficient
rj_numerator= mean((z_x-mean(z_x))*(z_x_plus-mean(z_x_plus)))
rj_denominator=mean((z_x-mean(z_x))^2)
rj_denominator_plus=mean((z_x_plus-mean(z_x_plus))^2)
rj=rj_numerator/sqrt(rj_denominator*rj_denominator_plus)
sc_test_score=rj*sqrt(length(unlist(z_score))-1)
cat("test score: ", sc_test_score)
cat("critical value: ", qnorm(0.05))
#conclude: reject hypothesis

##vi. Serial correlation test for nth lag
i=1
sc_test_score=-2
cat("critical value: ", qnorm(0.05))
while ((sc_test_score<qnorm(0.05)) | (qnorm(0.95)<sc_test_score)){
  z_x=z_score[1:(547-i)]
  z_x_plus=z_score[(1+i):547]
  #calculating serial coefficient
  rj_numerator= mean((z_x-mean(z_x))*(z_x_plus-mean(z_x_plus)))
  rj_denominator=mean((z_x-mean(z_x))^2)
  rj_denominator_plus=mean((z_x_plus-mean(z_x_plus))^2)
  rj=rj_numerator/sqrt(rj_denominator*rj_denominator_plus)
  sc_test_score=rj*sqrt(length(unlist(z_score))-1)
  cat("The test at lag",i,"has a test score of: ", sc_test_score,"\n")
  i=i+1
}
#Conclude: Failed to reject at lag 4
  

heart_failure_dataset <- read.csv("~/Downloads/heart_failure_clinical_records_dataset.csv")

#-----------------------------------------------------------------------------------------
#Q1 - Are diabetic peoples being more likely to have Blood Pressure?
#-----------------------------------------------------------------------------------------

# H0 : Diabetes is not associated with Blood Pressure 
# H1 : Diabetes is associated with Blood Pressure 
# alpha = 0.05

#----------Assumption 1 - By taking the whole data of both columns------------

# Results : X-squared = 0.0094767, df = 1, p-value = 0.9224
# Conclusion : we fail to reject H0
# No, Diabetic peoples are not more likely to have Blood Pressure?
test_data_Q1 <- table(heart_failure_dataset$diabetes, heart_failure_dataset$high_blood_pressure)
test_data_Q1
chi_test_Q1 <- chisq.test(test_data_Q1)
chi_test_Q1
barplot(test_data_Q1, col = c("#E69F00", "#56B4E9"))

#---------Assumption 2 - By taking just the diabetic patients-----------

# Results : X-squared = 12.168, df = 1, p-value = 0.0004862
# Conclusion : we reject H0
# Yes, Diabetic peoples are more likely to have Blood Pressure.

#creating a subset of diabetic people
diabetic_subset = heart_failure_dataset[heart_failure_dataset$diabetes == 1 ,]

test_data_Q1 <- table(diabetic_subset$high_blood_pressure)
test_data_Q1
chi_test_Q1 <- chisq.test(test_data_Q1)
chi_test_Q1
barplot(test_data_Q1 ,col = c("#E69F00", "#56B4E9"))


#-----------------------------------------------------------------------------------------
#Q2 - Are people having anemia are more likely to be from female?
#-----------------------------------------------------------------------------------------

# H0 : Anemia is not associated with gender
# H1 : Anemia is associated with gender
# alpha = 0.05

#----------Assumption 1 - By taking the whole data of both columns------------

# Results : X-squared = 2.2995, df = 1, p-value = 0.1294
# we fail reject H0
# No, people having anemia are not more likely to be from female

test_data_Q2 <- table(heart_failure_dataset$anaemia, heart_failure_dataset$sex)
test_data_Q2
chi_test_Q2 <- chisq.test(test_data_Q2)
chi_test_Q2
barplot(test_data_Q2,col = c("#E69F00", "#56B4E9"))


#----------Assumption 2 - By taking just the anemia patients------------

# Results : X-squared = 4.845, df = 1, p-value = 0.02773
# we reject H0
# Yes, people having anemia are more likely to be from female

#creating a subset of anemia
anemia_subset = heart_failure_dataset[heart_failure_dataset$anaemia == 1,]

test_data_Q2 <- table(anemia_subset$sex)
test_data_Q2
chi_test_Q2 <- chisq.test(test_data_Q2)
chi_test_Q2
barplot(test_data_Q2,col = c("#E69F00", "#56B4E9"))


#-----------------------------------------------------------------------------------------
#Q3 - Are people whose age is in between 55 and 65 having high Blood Pressure are less likely to survive?
#-----------------------------------------------------------------------------------------
# H0 : Age and blood pressure has no effect on survival
# H1 : Age and blood pressure are associated with survival
# alpha = 0.05
# Results : p-value = 0.2114
# we fail to reject H0
# people whose age is in between 55 and 65 having high Blood Pressure are not less likely to survive

#creating a subset of age between 55 and 65 having high blood pressure
age_subset = heart_failure_dataset[heart_failure_dataset$age > 55 & heart_failure_dataset$age < 65 & heart_failure_dataset$high_blood_pressure == 1,]

test_data_Q3 <- table(age_subset$DEATH_EVENT)
test_data_Q3
chi_test_Q3 <- chisq.test(test_data_Q3)
chi_test_Q3
barplot(test_data_Q3,col = c("#E69F00", "#56B4E9"))



#-----------------------------------------------------------------------------------------
#Q4 - Are diabetic peoples who smokes are less likely to survive?
#-----------------------------------------------------------------------------------------
# H0 : Diabetes and smoking are not associated with survival
# H1 : Diabetes and smoking are associated with survival
# alpha = 0.05
# Results : X-squared = 1.2, df = 1, p-value = 0.2733
# We fail to reject H0
# Diabetic peoples who smokes are not less likely to survive.

# diabetic subset
diabetes_subset = heart_failure_dataset[heart_failure_dataset$diabetes == 1 & heart_failure_dataset$smoking == 1,]

test_data_Q4 <- table(diabetes_subset$DEATH_EVENT)
test_data_Q4
chi_test_Q4 <- chisq.test(test_data_Q4)
chi_test_Q4
barplot(test_data_Q4,col = c("#E69F00", "#56B4E9"))


#-----------------------------------------------------------------------------------------
#Q5 - Are male people being more likely to get diabetic?
#-----------------------------------------------------------------------------------------
# H0 : male and female equally get diabetic
# H1 : male and female not equally get diabetic
# alpha = 0.05
# Results : p-value = 0.0001058
# we reject H0
# Yes, male people are more likely to get diabetic

#creating a subset of male
male_subset = heart_failure_dataset[heart_failure_dataset$sex == 1 ,]

test_data_Q5 <- table(male_subset$diabetes)
test_data_Q5
chi_test_Q5 <- chisq.test(test_data_Q5)
chi_test_Q5
barplot(test_data_Q5,  col = c("#E69F00", "#56B4E9"))

#............................................E N D...........................................#









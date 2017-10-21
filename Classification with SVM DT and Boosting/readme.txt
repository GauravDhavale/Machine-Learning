
1: Project required two csv files adult.csv and HR.csv.

2: To run the project, just keep both the CSV files and project code in the same directory. 
   
3: The project needs write permission in the directory where project code is kept. The Project will write two CSV files that contain SMOTE function output. These files will be referred by h2o package for boosting algorithm. 
 
4: The project will check if the package is available or not and will install if its missing before referring.

5: The Packages used in the project are as follows:

library(DMwR) 
library(caret)
library(caTools)
library(e1071)
library(rattle)
library(rpart)
library(rpart.plot)
library(ROCR)
library(ggplot2, quietly=TRUE)
library(h2o)

6. The project will print the output as well as operation its performing as a status in console. It will take approx. 35 min to complete.

7. Project report is Report.docx file
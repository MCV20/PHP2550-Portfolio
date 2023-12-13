# PHP 2550 Project 2

## Description 

This study delves into determining the most effective criteria and timing for tracheostomy placement in neonates grappling with severe bronchopulmonary dysplasia (sBPD). The primary objective revolves around crafting a predictive regression model that factors in the combined outcome of tracheostomy or death. This composite outcome paints a comprehensive picture of patient outcomes, encapsulating critical events that are pivotal in understanding the severity and risks associated with sBPD. The study constructed predictive models at three pivotal time points in an infant's life: birth, 36 weeks, and 44 weeks. A comparative analysis between two statistical methodologies—the mixed model and the LASSO model—was conducted. Across these time points, the mixed model consistently demonstrated superior performance, showcasing higher AUC scores and offering a more streamlined and interpretable parameterization, especially post application of the StepAIC criterion.

It's noteworthy that prenatal steroids emerged as a consistently significant factor across all models. Variables such as gestational age, birth length, ventilation support, and medication for pulmonary hypertension were identified as crucial predictors for tracheostomy outcomes at different time points. The study's recommendation leans towards adopting the mixed model for prediction at each time point due to its distinctive strengths and performance characteristics demonstrated across varied stages in the predictive modeling process. This comprehensive and robust approach aims to empower healthcare professionals with accurate and insightful tools for managing infants with sBPD, enhancing decision-making and patient care strategies.


## Guideline

In this folder you will find 3 different folders. The folder `Data` contains the codebook for the data used in this project. The folder `Manuscript` contains the pdf report and the zip-file for the Latex text. The `R Code` folder contains the code for reproduction. You will find 4 files there.the file `project2-try.R` contains exploratory analysis and some visualization plots not used in the report. The file `project2.qmd` contains all necessary code for reproduction. Additionally, the file `glmmlasso-project2-36WK.R` and `glmmlasso-project2-BIRTHMODEL.R` contains some code for the LASSO model with mixed effects. However, a bug in the code could not be fixed and hence this was not incldued in the final report. 



# PHP 2550 Project 3

## Description 

The study revolves around the critical challenge of ensuring the effectiveness of predictive models across different populations—a concept known as model transportability. Developing models in one population and applying them to another is essential in healthcare decision-making, but discrepancies between populations can impact model performance. Strategies involving methodologies and validation techniques are crucial to bridge this gap between the population used for model development and the one where it's deployed. Issues arise when the target population lacks specific outcome details, preventing direct creation or evaluation of prediction models. To address this, leveraging covariate and outcome data from the source population while adjusting for differences between populations becomes an appealing option. The investigation assesses the efficacy of cardiovascular prediction models developed in the Framingham dataset when applied to NHANES, lacking specific outcome data. The study aims to understand if these models remain effective across diverse populations by evaluating their performance within NHANES demographics. This study focuses on estimating the Brier score for cardiovascular disease prediction models stratified by sex in a population lacking specific outcome data. Analyzing the NHANES dataset shows a lower estimated Brier score than the source population, potentially due to differences in health status. While evaluating simulated data within NHANES, both men and women show elevated Brier scores, suggesting possible discrepancies in capturing underlying relationships.

The precision in estimation processes indicates stability and consistency, reflecting reliable results. The alignment between estimated and actual Brier scores hints at these models' potential transportability and adaptability to diverse populations. Tailoring models to specific contexts might further enhance predictive accuracy.Proposed methods involve estimating source population membership likelihood and creating weights to tailor predictive models for the target population, potentially improving performance in new contexts.



## Guideline

In this folder you will find 3 different folders.The folder `Manuscript` contains the pdf report and the zip-file for the Latex text. The `R Code` folder contains the code for reproduction. You will find two files there. The file `project3.qmd` contains all necessary code for reproduction and the file `project3-fixed-res.RData` contains the saved workspace of the simulation results.



# Heart-Disease-Risk-Prediction
# Overview
This project revolves around predicting the risk of heart disease based on health characteristics using the Framingham Heart Study dataset from 2019. The dataset includes information on 4000 patients, encompassing variables such as age, smoking habits, blood pressure, diabetes, and cholesterol levels.
# Objective
The primary goal is to assist medical professionals in predicting heart disease risk using common health records. Our analysis highlights key contributors to risk, enabling targeted preventive measures.
# Findings
Supervised Learning

-Logistic Regression

The model excels at predicting patients not at risk (specificity 99.3%).
Prior stroke, cigarettes per day, age, and systolic blood pressure are significant predictors.
Recommendations include cautious risk prediction due to varying model accuracy.

-K Nearest Neighbors (kNN)

Accurate at predicting patients not at risk (specificity 96.79%).
Limited effectiveness in predicting at-risk patients (sensitivity 11.47%).
Beneficial for excluding individuals not at risk.

-Naive Bayes

Strong at predicting patients not at risk (accuracy 91.09%).
Limited accuracy in predicting at-risk patients (sensitivity 25.94%).
Valuable for ruling out heart disease risk.

-Classification Trees

Age and systolic blood pressure are top predictors.
Highly accurate in identifying patients not at risk (specificity 97.46%).
Limited accuracy in predicting at-risk patients (sensitivity 12.12%).

Unsupervised Learning

-Clustering

Three distinct clusters based on smoking habits, diabetes, and stroke history.
Cluster analysis reveals diabetes, prior stroke, and smoking as significant risk factors.
Cluster 3, representing patients with diabetes, suggests the highest risk of heart disease.
# Recommendations

-Caution in Predictions:

Acknowledge model variations in predicting heart disease risk.
Emphasize careful risk assessment, especially for at-risk patients.

-Intensive Blood Pressure Medication:

Increase prescription of blood pressure medication for patients with high systolic blood pressure.
Focus on Diabetes and Prior Stroke:
Recognize the uncontrollable nature of diabetes and prior stroke.
Direct attention to other contributing factors for patients with these characteristics.

-Research on Vaping:

Acknowledge the shift to vaping among younger generations.
Propose extensive research on vaping implications for heart disease and overall health.
# Conclusion
Our comprehensive analysis provides insights into predicting heart disease risk. The combination of supervised and unsupervised learning techniques reveals critical factors influencing predictions. Medical professionals can leverage these findings for targeted interventions and patient care.

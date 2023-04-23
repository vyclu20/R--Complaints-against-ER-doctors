# R--Complaints-against-ER-doctors

Project was last updated on: 6 November 2022

**This was a university project for an analysis of observations unit.**

Complaints in healthcare, specifically towards doctors who work in emergency, feel as though the events where they receive oppositions taken by others are perpetuated or out of the norm. However, some analysis revealed patterns as for certain demographics may play a part in the number of complaints a doctor gets. In this article, a dataset of 94 hospital doctors who worked in an emergency department, was used to examine how variables such as gender, revenue, visits, residency status and number of hours worked, all recorded over the last year data, have any impact on the number of complaints filed against ER doctors. 

The main findings of this study were that a zero inflated negative binomial model with variables visits, revenue, hours and gender without interactions were able to best predict the likelihood or the counts odds of a doctor receiving more complaints. Furthermore, it was discovered that the variable visit was the most significant in determining whether a doctor were to receive more complaints, followed by the revenue. Additionally, it was found that female doctors are 69.775% less likely to get complaints than male doctors, and the fact that doctors generally receive fewer complaints when they work more hours. It is worth mentioning that the variable revenue has a negative coefficient, as do the variable hours. 

For the final model, goodness of fit was determined using likelihood tests of the zero inflated negative binomial models of similar fit in which the simpler one was chosen. Furthermore, formulas for the count and the Bernoulli/binomial process (with complaints as the response variable) of the zero negative binomial models will be utilized to examine and better understand the effects of how certain doctor information has on an ER doctor’s complaint volume. 

## Project brief

The file compdat.txt contains data on the number of complaints received, along withsome demographic information, for 94 doctors who worked in an emergency service at a
hospital.

The following are the variables in the data.

> visits: the number of patient visits

> complaints: the number of complaints against the doctor in the previous year

> residency: is the doctor in residency training (Y = Yes, N = No)

> gender: gender of the doctor (M = male, F = female)

> revenue: doctor’s hourly income (dollars)

> hours: total number of hours the doctor worked in a year

Aim of the analysis: To determine the effect of the variables on the number of complaints received.

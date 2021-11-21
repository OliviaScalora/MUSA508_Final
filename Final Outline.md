# MUSA 508 Final Project 

## Predict heroin overdose events to better allocate prevention resources 

The City of Mesa has a dataset of heroin overdose locations. Using these data extracted from the city’s [Open Data portal](https://data.mesaaz.gov/Fire-and-Medical/Fire-and-Medical-Opioid-Overdose-Incidents/qufy-tzv6), your job will be to estimate a geospatial risk prediction model, predicting overdoses as a function of environmental factors like crime, 311 and inspections. You should validate your model against a kernel density, as we have did in class. Also, you should try to train your model from one time period (long enough to have enough data) and test it on an out of out of sample test set time period (the following year, for instance). Note the fact that the data have some accuracy diminished to make them more anonymous – think about how this plays into your prediction and use case. 

You can also undertake this project using [similar data from Cincinnati, Ohio.](https://github.com/sydng/Cincinatti_Overdose_Data)

Think critically about how you might offer these predictions to a public health official in your app. What do they want to know? Also remember that while your predictions are about overdose, it may be safe to assume that these are also places where people are just using heroin. 

<br></br>
## Project Timeline 


### 11/26 Weekend Zoom meeting   


### 11/29 Meet midday 


### 12/3 
##### Preliminary research 
*   examples of prediction apps  
*   existing programs in Mesa 
##### App Concept Development   
*   answer use case questions  
*   wireframes 


### 12/10 
##### Olivia -Exploratory Data Analysis 
*   Wrangle Data  
*   Build Model  
##### Hasa- Presentation Prep   
*   create slides 
*   tell concise story of our concept 


### 12/17 
##### Olivia - Create Markdown 
##### Hasa - Record Video 

<br></br>
## Use Case Questions 

<ol>
    
#### <li> What is the use case?  </li>
    
Predict heroin overdose events to better allocate prevention resources.  
Create an application that will help health officials allocate prevention resources.
    
#### <li> How could data make a difference in answering this question? Do you have a sense for the business as usual decision making? </li>

Mesa is doing a lot of research on Opiod crisis already – how is our model improving on the current system? [*What does opioid overdose look like in Mesa?*](https://data.mesaaz.gov/stories/s/Opioid-Overdose-A-Public-Health-Emergency/ma3e-anqw/)   
In 2020, the problem of opioid overdose incidents in Mesa increased 76% and the number of deaths as a result of opioid overdose grew 132%.   
Where are opioid overdoses occurring in Mesa? Map shows approximate locations and are rounded to 1/3 mile increments – points do not represent actual locations of incidents. Map shows aggregate # of overdose incidents per block group  
[MaricopaRx](https://www.maricoparx.org/) is an online tool to connect treatment providers, educators, community-based organizers, law enforcement, and other stakeholders   

#### <li>What datasets have you identified to help you answer this question? </li>

*  **Fire and Medical Opioid Overdose Incidents** [Mesa Open Data Portal](https://data.mesaaz.gov/Fire-and-Medical/Fire-and-Medical-Opioid-Overdose-Incidents/qufy-tzv6) - Confirmed cases of opioid overdose, locations are not actual but instead rounded to approximately 1/3 mile increments. Opioid overdose confirmed by 1) patient or witness verification, 2) Opioid found on scene or 3) positive response to Narcan treatment.

 
#### <li>What kind of model would you build and what is the dependent variable? </li>


#### <li>How will you validate this model (cross-validation & goodness of fit metrics that relate to the business process)? </li>


#### <li>How do you think that stakeholders would want to consume this data? </li>


#### <li>What are the use cases for your app and what should the app do? </li>
    
</ol>


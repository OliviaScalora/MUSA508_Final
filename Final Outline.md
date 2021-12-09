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
*   Examples of prediction apps  
*   Existing programs in Mesa 
##### App Concept Development   
*   Answer use case questions  
*   Wireframes 


### 12/10 
##### Olivia -Exploratory Data Analysis 
*   Wrangle & manipulate data  
*   Build model  
##### Hasa- Presentation Prep   
*   Create slides 
*   Tell concise story of our concept 


### 12/17 
##### Olivia - Create Markdown 
##### Hasa - Record Video 

<br></br>
## Use Case Questions 

<ol>
    
#### <li> What is the use case?  </li>
    
Create a model that predicts where opioid overdoses are most likely to occur in Mesa AZ. 
Propose creation of new program - Opioid Observation and Treatment Center (OOTC). 
OOTC will serve two purposes  
    
* Be a safe space that provides support, safe - monitoring, overdose treatment, and primary care for individuals who are using and would otherwise be on the street.   
* Have staff and resources prepared for emergency response to overdose incidents -- cater to public and residential overdoses.  
    
Create a user-friendly web application accessible to healthcare officials that visualizes and describes the best 
locations for potential Opioid Observation and Treatment Centers (OOTC)
    
City and healthcare officials can properly allocate staff and resources to creating these centers with the goal of mitigating opioid overdose death. 
    
#### <li> How could data make a difference in answering this question? Do you have a sense for the business as usual decision making? </li>

Mesa is doing a lot of research on Opiod crisis already – how is our model improving on the current system? [*What does opioid overdose look like in Mesa?*](https://data.mesaaz.gov/stories/s/Opioid-Overdose-A-Public-Health-Emergency/ma3e-anqw/)   
In 2020, the problem of opioid overdose incidents in Mesa increased 76% and the number of deaths as a result of opioid overdose grew 132%.   
Where are opioid overdoses occurring in Mesa? Map shows approximate locations and are rounded to 1/3 mile increments – points do not represent actual locations of incidents. Map shows aggregate # of overdose incidents per block group  
[MaricopaRx](https://www.maricoparx.org/) is an online tool to connect treatment providers, educators, community-based organizers, law enforcement, and other stakeholders   

#### <li>What datasets have you identified to help you answer this question? </li>

*   [Fire and Medical Opioid Overdose Incidents](https://data.mesaaz.gov/Fire-and-Medical/Fire-and-Medical-Opioid-Overdose-Incidents/qufy-tzv6) - Confirmed cases of opioid overdose, locations are not actual but instead rounded to approximately 1/3 mile increments. Opioid overdose confirmed by 1) patient or witness verification, 2) Opioid found on scene or 3) positive response to Narcan treatment.
*   [Census ACS data](https://www.census.gov/programs-surveys/acs) - demographic and socio-economic indicators at the block group level
*   [Park Locations And Amenities](https://data.mesaaz.gov/Parks-Recreation-and-Community-Facilities/Parks-Locations-And-Amenities/djym-pkpp) - Information about location and amenities available at developed city parks, park open spaces (basins) maintained by the City of Mesa. Pools, Recreation Centers and other public gathering facilities owned and/or maintained by the City of Mesa's Parks Department such as convention center, amphitheater and cemetery are also listed.
*   [UFB Food Distribution 2021l](https://data.mesaaz.gov/Parks-Recreation-and-Community-Facilities/UFB-Food-Distribution-2021/mf65-6nx7) -Information about United Food Bank (UFB) Food Distribution 2021 in partnership with the City of Mesa.
*   [Student Demographics - Mesa Public Schools](https://data.mesaaz.gov/Neighborhoods/Student-Demographics-Mesa-Public-Schools/svjb-9ytu) - Students Demographics for Mesa Public Schools by School Year and Grade Level since 1979.
*   [Light Rail Ridership](https://data.mesaaz.gov/Transit-Services/Light-Rail-Ridership/pdpe-wbxu) - For station locations - Breakdown of light rail ridership on a monthly basis per station in Mesa (data lags for approximately 30 days) and is provided by Valley Metro, an external agency who is responsible for both light rail and bus services in Mesa.
*   [Bus Ridership](https://data.mesaaz.gov/Transit-Services/Bus-Ridership/nmjv-498y) - For bus stop locations - Breakdown of bus service ridership on a monthly basis per route in Mesa (data lags for approximately 30 days) and data is provided by Valley Metro, an external agency who is responsible for both bus service and light rail in Mesa.
*   [City Boundary Map](https://data.mesaaz.gov/Zoning-Property/City-Boundary-Map/wf8n-kwgk) 
*   [Zoning Districs](https://data.mesaaz.gov/Zoning-Property/Zoning-Districts/qscf-6ebm) - Specifically delineated geographic areas in the city within which regulations and requirements uniformly govern the use of land
 
#### <li>What kind of model would you build and what is the dependent variable? </li>

We will use a geospatial risk model to predict heroin overdoses as a function of environmental factors (see data above) 

#### <li>How will you validate this model (cross-validation & goodness of fit metrics that relate to the business process)? </li>

The model will be validated against a kernal density. Train the model for the year of 2018 and test on the year 2019. The city saw a huge spike in overdoses between 2019 and 2020 so it would not make sense to train and test between these years, however the 2020 spike is something to take into account for forecasting. TBD...
    
#### <li>How do you think that stakeholders would want to consume this data? </li>

Who are our stakeholders? - Policy makers, Healthcare Professionals, Fire and Medical Department
interactive map and charts - legible data visualizations

#### <li>What are the use cases for your app and what should the app do? </li>

App ideas:
*   Desktop interface
*   Home page accesible to the public - login feature for official use by fire and medical department
*   Public home page offers informational panels and resources for individuals seeking addiction help as well as friends and family of addicts 
*   Public page displays simple statistics of the opioid crisis of Mesa - how it has changed over time, where the numbers are now - bring awareness
*   Login page for official use - more in depth access to data and predictive model
*   A map is displayed of where overdose hotspots have been historically and how our model predicts overdose trends to change spatially
    
</ol>

## Deliverables

#### Presentation - Due 12/10
Team member 1 will be responsible for a 4 minute 'PechaKucha' presentation that ‘sells’ us on the idea of this fancy new planning app that you’ve designed to solve an important problem. Spend ~50% of your time on exploratory analysis and model results/validation. The expectation is that you will have preliminary model at this stage, which you will sharpen by the time the assignment is due. The other 50% should focus on questions like, What is the use case? Who is the user? How does the app put the model into the hands of a non-technical decision maker? Who is creating the app? Have you created something that is usable by the client? This is a presentation where the slides are set to change automatically, every 20 seconds. This is a requirement. Remember – sell it to us. What should come first - the model or the app? Don’t forget to constantly remind the audience about the use case to keep your solution relevant. 

#### Presentation - Due 12/17 - noon
Team member 1 will have the pechakucha uploaded on youtube with a recorded narration. Link to the video in your markdown.

#### R Markdown - Due 12/17 - noon

Team member 2 will be responsible for a markdown write up that would allow someone to replicate your analysis (show your code blocks). Post this markdown on your Github not in a google folder. At minimum, please hit on the below components:

a. Motivate the analysis – “What is the use case; why would someone want to replicate your analysis and why would they use this approach?”  
b. Describe the data you used.  
c. Describe your exploratory analysis using maps and plots.  
d. What is the spatial or space/time process?  
d. Describe your modeling approach and show how you arrived at your final model.  
e. Validate your model with cross-validation and describe how your predictions are useful (accuracy vs. generalizability).  
f. Provide additional maps and data visualizations to show that your model is useful.  
g. Talk about how your analysis meets the use case you set out to address.  
h. What could you do to make the analysis better?  


# MUSA 508 Final Project 

**[Youtube link to final presentation](https://www.youtube.com/watch?v=dKI847qx-gI)**  
**[Link to Final Markdown](https://oliviascalora.github.io/Mesa-Opioid-Prediction/)**

## Predict heroin overdose events to better allocate prevention resources 

The City of Mesa has a dataset of heroin overdose locations. Using these data extracted from the city’s [Open Data portal](https://data.mesaaz.gov/Fire-and-Medical/Fire-and-Medical-Opioid-Overdose-Incidents/qufy-tzv6), your job will be to estimate a geospatial risk prediction model, predicting overdoses as a function of environmental factors like crime, 311 and inspections. You should validate your model against a kernel density, as we have did in class. Also, you should try to train your model from one time period (long enough to have enough data) and test it on an out of out of sample test set time period (the following year, for instance). Note the fact that the data have some accuracy diminished to make them more anonymous – think about how this plays into your prediction and use case. 

You can also undertake this project using [similar data from Cincinnati, Ohio.](https://github.com/sydng/Cincinatti_Overdose_Data)

Think critically about how you might offer these predictions to a public health official in your app. What do they want to know? Also remember that while your predictions are about overdose, it may be safe to assume that these are also places where people are just using heroin. 
<br></br>
## Use Case Questions 

<ol>
    
#### <li> What is the use case?  </li>
    
   Create a model that predicts where opioid overdoses are most likely to occur in Mesa AZ. 
Propose creation of new program - Opioid Observation and Treatment Center (OOTC). 
OOTC will serve two purposes  
    
* Be a safe space that provides support, safe - monitoring/safe injection site, overdose treatment, and primary care for individuals who are using and would otherwise be on the street.   
* Have staff and resources prepared for emergency response to overdose incidents -- cater to public and residential overdoses.   
    
  Conceptualize a user-friendly web application accessible to healthcare officials that visualizes the urgency for these new centers and describes the best locations for potential Opioid Observation and Treatment Centers (OOTC). City and healthcare officials can properly allocate staff and resources to creating these centers with the goal of mitigating opioid overdose death. 
    
#### <li> How could data make a difference in answering this question? Do you have a sense for the business as usual decision making? </li>

Mesa is doing a lot of research on Opiod crisis already – how is our model improving on the current system? [*What does opioid overdose look like in Mesa?*](https://data.mesaaz.gov/stories/s/Opioid-Overdose-A-Public-Health-Emergency/ma3e-anqw/)   
In 2020, the problem of opioid overdose incidents in Mesa increased 76% and the number of deaths as a result of opioid overdose grew 132%.   
Where are opioid overdoses occurring in Mesa? Map shows approximate locations and are rounded to 1/3 mile increments – points do not represent actual locations of incidents. Map shows aggregate # of overdose incidents per block group  
[MaricopaRx](https://www.maricoparx.org/) is an online tool to connect treatment providers, educators, community-based organizers, law enforcement, and other stakeholders   
    
Arizona opioid plan solutions  
Arizona’s Healthcare Cost Containment System (AHCCCS) - leading payer for substance absue treatment in the state – 2015 - $162,939,257 to 2017 - $236,316,548 (69% increase in investments) 
Department of Child Safety (DCS)– offers substance abuse treatment to parents who are working toward reunification when addiction is a factor 
Department of Corrections (ADC) - provides additional substance abuse treatment to inmates 
Collectively, the stat invests $265 million annually in substance abuse treatment and prevention 
    
The problem-  
inconsistent community care – individuals seeking treatment are not always elligible to reside in licenses behavioral health residential facilities or recovery homes 
overdose treatment – focuses on immediate treatment, does not treat underlying issue -  only 47% of overdose patients are referred to behavioral health providers upon release 
Gaps in treatment facility access – there are still people not elligible for AHCCCS that are uninsured or underinsured and are unable to receive treatment due to cost 
    
#### <li>What datasets have you identified to help you answer this question? </li>

* [**Fire and Medical Opioid Overdose Incidents**](https://data.mesaaz.gov/Fire-and-Medical/Fire-and-Medical-Opioid-Overdose-Incidents/qufy-tzv6) Confirmed cases of opioid overdose, locations are not actual but instead rounded to approximately 1/3 mile increments. Opioid overdose confirmed by 1) patient or witness verification, 2) Opioid found on scene or 3) positive response to Narcan treatment.
* [**City Property Data**](https://data.mesaaz.gov/Zoning-Property/City-Owned-Property/xms2-ya86) will be filtered to account for various facility type - child crisis center, fire and police station, public housing, arts and education centers.
* [**Crime Data**](https://data.mesaaz.gov/Police/Police-Incidents/39rt-2rfj) Incidents based on initial police reports taken by officers when responding to calls for service. Data is modified for public use. Address and Location are not exact locations of incidents and have been rounded to nearest hundred block. Lat/Long are approximations only based on rounded hundred block. Incidents reported in this dataset may not correlate with 911 Events datasets and calls for Police service. The City of Mesa does not disclose information that is inflammatory in nature that impacts our citizens. Split by type; misdemeanors and felonies.
* [**Park Locations And Amenities**](https://data.mesaaz.gov/Parks-Recreation-and-Community-Facilities/Parks-Locations-And-Amenities/djym-pkpp) Information about location and amenities available at developed city parks, park open spaces (basins) maintained by the City of Mesa. Pools, Recreation Centers and other public gathering facilities owned and/or maintained by the City of Mesa's Parks Department such as convention center, amphitheater and cemetery are also listed.
* [**Light Rail Stations**](https://opengis.mesaaz.gov/datasets/7c6b201d9e38451185032fec51acfaa4_0/explore) This dataset provides information regarding the location of current light rail stations within the City of Mesa and each station’s name and address.
* [**City Boundary Map**](https://data.mesaaz.gov/Zoning-Property/City-Boundary-Map/wf8n-kwgk) City of Mesa jurisdiction.
* [**Zoning Districs**](https://data.mesaaz.gov/Zoning-Property/Zoning-Districts/qscf-6ebm) Specifically delineated geographic areas in the city within which regulations and requirements uniformly govern the use of land - separated between High density residential, low density residential, downtown, commercial, and industrial.
* [**Census ACS data**](https://www.census.gov/programs-surveys/acs) demographic and socio-economic indicators at the tract level.
 
#### <li>What kind of model would you build and what is the dependent variable? </li>

Geospatial risk model- Poisson Regression- to predict heroin overdoses as a function of environmental factors (see data above) 

#### <li>How will you validate this model (cross-validation & goodness of fit metrics that relate to the business process)? </li>

Kfold and Leave One Group Out(LOGO) Cross Validation - test with and without variable to account for spatial process.
    
#### <li>How do you think that stakeholders would want to consume this data? </li>

Who are our stakeholders? - Policy makers, Healthcare Professionals, Fire and Medical Department
interactive map and charts - legible data visualizations

#### <li>What are the use cases for your app and what should the app do? </li>

App ideas:
*   Desktop interface
*   Home page accesible to the public - login feature for official use by city health officials
*   Public home page offers informational panels and resources for individuals seeking addiction help as well as friends and family of addicts 
*   Log in feature displays interactive web map
*   A map displayed of various features - historic hotspots, predicted hot spots, and recommended commercial blocks where the implementation of OOTC is most urgent according to our predictions. 
*   App will have tab for downloadable raw data and a link to the code repository for anyone looking to replicate the analysis. 
    
</ol>


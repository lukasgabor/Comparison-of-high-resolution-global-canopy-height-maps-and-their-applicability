# Comparison-of-high-resolution-global-canopy-height-maps-and-their-applicability

This repository was created as a supporting material for the article comparing high-resolution global canopy height maps and exploring their applicability to biodiversity modeling in temperate biomes.

**What is the research about?**
Being directly linked to aboveground biomass and biodiversity, forest canopy height is a high-priority variable to be monitored from space. Recently, two global canopy height maps have been released, the Global Forest Canopy Height Map (GFCH) and the High-Resolution Canopy Height Model of the Earth (HRCH). We assessed their accuracy using canopy height models derived from airborne laser scanning in three areas of the temperate biome. Our results show the presence of large errors in the evaluated maps. The RMSE of GFCH was between 7-8 m while the RMSE of HRCH was between 10-16 m. HRCH was less sensitive to variation in vegetation height, resulting in poor estimates of canopy horizontal heterogeneity. Canopy height estimates in both maps were equally affected by terrain slope and orientation. Finally, biodiversity models using global canopy height maps lead to a considerable decrease in models’ discrimination ability and to mischaracterization of species niches.

**Relevant paper:**
Here I will add the paper citation

Code authors: [Vítězslav Moudrý](https://scholar.google.cz/citations?user=aSI2lNEAAAAJ&hl=cs),
              [Jiří Prošek](https://scholar.google.cz/citations?user=TMVQtt4AAAAJ&hl=cs),
              [Lukáš Gábor](https://scholar.google.cz/citations?user=pLQXY5wAAAAJ&hl=cs)

Cite the code:

Cite the data:
              
Date: 11/15/2022

**Environmental and species data**

***Workflow 1 - virtual environmental data + virtual species***

As both, environemtal and species data have been simulated there is no need to download any. Just follow attached R script. 

***Workflow 2 - real environmental data + virtual species***

Environmental data are available for download on [Download Center of Autonomous body National Center for Geographic Information web site](https://centrodedescargas.cnig.es/CentroDescargas/locale?request_locale=en#) (amount of forest, amount of grassland, elevation). Aspect and topography wetness index were derived from the elevation.

***Workflow 3 - real environmental data + real species***

Band-tailed Pigeon dataset can be downloaded [here](https://www.sciencebase.gov/catalog/item/5eb4485182ce25b5135abeea). Environmental data used for Band-tailed Pigeon SDMs are available for download on [modis](https://modis.gsfc.nasa.gov/) (Mean winter EVI, ), [chelsea](https://chelsa-climate.org/) (Seasonality of precipitation, Mean annual temperature, ), [earthenv](https://www.earthenv.org/) (Cloud cover, Terrain ruggedness, EVI spatial heterogeneity), [soilgrids](https://www.soilgrids.org/) (Soil clay content, Soil silt content), [envidat](https://www.envidat.ch/#/) (Growing season precipitation).

**File Description:**

R_script_workflow1: Step-by-step guideline which allow replication of Workflow 1 from generating virtual data to plotting results. 

R_script_workflow2: Step-by-step guideline which allow replication of Workflow 2 from generating virtual species to plotting results. 

R_script_workflow3: Step-by-step guideline which allow replication of Workflow 3 from simulating positional error in occurrence data to plotting results.

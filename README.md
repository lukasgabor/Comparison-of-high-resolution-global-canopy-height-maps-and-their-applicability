# Comparison-of-high-resolution-global-canopy-height-maps-and-their-applicability

This repository was created to provide supporting material for an article comparing high-resolution global canopy height maps and exploring their applicability to biodiversity modeling in temperate biomes.

**What is the research about?**

Being directly linked to aboveground biomass and biodiversity, forest canopy height is a high-priority variable to be monitored from space. Recently, two global canopy height maps have been released, the Global Forest Canopy Height Map (GFCH) and the High-Resolution Canopy Height Model of the Earth (HRCH). We assessed their accuracy using canopy height models derived from airborne laser scanning in three areas of the temperate biome. Our results show the presence of large errors in the evaluated maps. The RMSE of GFCH was between 7-8 m while the RMSE of HRCH was between 10-16 m. HRCH was less sensitive to variation in vegetation height, resulting in poor estimates of canopy horizontal heterogeneity. Canopy height estimates in both maps were equally affected by terrain slope and orientation. Finally, biodiversity models using global canopy height maps lead to a considerable decrease in models’ discrimination ability and to mischaracterization of species niches.

**Relevant paper:**
Here I will add the paper citation
              
Date: 11/17/2022

**Canopy Height Data**

The original airborne laser scanning data (ALS) used for the generation of the canopy height models were sourced from the [LINZ Data Service](https://data.linz.govt.nz/) and licensed for reuse under the CC BY 4.0 licence; the [Federal Office of Topography swisstopo](https://www.swisstopo.admin.ch/en/geodata/height/surface3d.html); and the [U.S. Geological Survey](https://apps.nationalmap.gov/downloader/).

The [Global Forest Canopy Height Map](https://glad.umd.edu/dataset/gedi) (GFCH, Potapov et al. 2021) and the [high-resolution canopy height model of the Earth](https://langnico.github.io/globalcanopyheight/) (HRCH, Lang et al. 2022) are provided free of charge, without restriction of use under Creative Commons Attribution 4.0 International License. Publications, models, and data products that make use of these datasets must include proper acknowledgement.

*P. Potapov, X. Li, A. Hernandez-Serna, A. Tyukavina, M.C. Hansen, A. Kommareddy, A. Pickens, S. Turubanova, H. Tang, C.E. Silva, J. Armston, R. Dubayah, J. B. Blair, M. Hofton (2021) Mapping and monitoring global forest canopy height through integration of GEDI and Landsat data. Remote Sensing of Environment, 112165. https://doi.org/10.1016/j.rse.2020.112165*

*Lang, N., Jetz, W., Schindler, K., & Wegner, J. D. (2022). A high-resolution canopy height model of the Earth. arXiv preprint arXiv:2204.08322.*

Used canopy height data can be downloaded from the Zenodo repository, where EBR stands for Entlebuch Biosphere Reserve, MRF stands for Mount Richmond Forest and TAW stands for Trinity Alps Wilderness.

***Cite the data***

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7332572.svg)](https://doi.org/10.5281/zenodo.7332572)

**Files Description:**

R_script_Raster_Profiles.R: Step-by-step guideline to generate profiles such as the examples in Figure 1b in the manuscript. This allows the readers to assess the accuracy of global canopy height maps within the three study areas.

R_script_Virtual_species_niche.R: Step-by-step guideline to generate the probability of species occurrence with respect to canopy height or habitat heterogeneity estimated with generalized linear models using ALS canopy height, GFCH map, and HRCH map

Code authors: [Vítězslav Moudrý](https://scholar.google.cz/citations?user=aSI2lNEAAAAJ&hl=cs), [Lukáš Gábor](https://scholar.google.cz/citations?user=pLQXY5wAAAAJ&hl=cs)

***Cite the code***

[![DOI](https://zenodo.org/badge/566486768.svg)](https://zenodo.org/badge/latestdoi/566486768)


#### Integrating community science and agency-collected monitoring data to expand monitoring capacity at large spatial scales

#### Contents of this repository

1.  Data Folder File with eBird data access query information and links
    to download site covariate data for use in ArcMap Washington
    Department of Fish and Wildlife Common Loon observation data from
    2017

2.  Scripts R scripts for filtering eBird data to map in ArcMap
    Formatting spatially filtered eBird data with WDFW data for use in
    the occupancy model Nimble multi-state occupancy model script to run
    the model Files to generate figures and parameter estimates

3.  Figures

**Data** eBird Basic Dataset is available through request at ebird.org
(<https://ebird.org/data/download>). The query for the basic dataset
used in the analysis was: Species: Common Loon Region: Washington State
Date Range: June 2000 to November 2017 Did not include unvetted data

Citation of the eBird data used in this manuscript: eBird Basic Dataset.
2017. EBD\_US-WA\_relNov-2017. Cornell Lab of Ornithology, Ithaca, New
York. eBird Sampling Dataset. 2017. EBD\_sampling\_relNov-2017. Cornell
Lab of Ornithology, Ithaca, New York.

State monitoring data: Washington Department of Fish and Wildlife data,
file in data folder: wdfw2017COLO.csv

Site covariate data (used in ArcMap): National Land Cover Database: 2016
data available for download at:
<https://www.usgs.gov/centers/eros/science/national-land-cover-database>
Land Cover types grouped together by category as described in the
manuscript.

Yang, L., S. Jin, P. Danielson, C. Homer, L. Gass, S. M. Bender, A.
Case, C. Costello, J. Dewitz, J. Fry, M. Funk, B. Granneman, G. C.
Liknes, M. Rigge, and G. Xian. 2018. A new generation of the United
States National Land Cover Database: requirements, research priorities,
design, and implementation strategies. ISPRS Journal of Photogrammetry
and Remote Sensing 146:108–123.

Human Influence Index: Human Influence Index dataset available for
download at:
<https://sedac.ciesin.columbia.edu/data/set/wildareas-v2-human-influence-index-geographic>

Wildlife Conservation Society. 2005. Global Human Influence Index (HII)
Dataset. Last of the Wild Project Version 2.

Elevation: Elevation data available for download at:  
<https://www.usgs.gov/the-national-map-data-delivery/gis-data-download>

U.S. Geological Survey. 2017. 3D Elevation Program 1-Meter Resolution
Digital Elevation Model.
<https://www.usgs.gov/core-science-systems/ngp/3dep/data-tools>.

National Hydrography Database: National Hydrography Dataset available
at:
<https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products>.

U.S. Geological Survey. 2018. National Hydrography Dataset (ver. USGS
National Hydrography Dataset Best Resolution (NHD) for Hydrologic Unit
(HU) 4 - 2001).

### Use of this repository

This repository -Framework for filtering and formatting eBird data for
use in an occupancy model, data sources for site covariate data. Note
that ArcMap was used to combine site-specific covariate data with
observation data (no script included for this step) -Nimble multi-state
occupancy model script utilizing both eBird and state monitoring data
-Figures and parameter estimates from MCMC output

**Scripts**

1.  EBird\_Auk\_filtering.R:

2.  SPfilteredEBird.R & FormattedData.R

3.  NimbleOccModel.R

4.  PosteriorsandTables.R, PredictionsandFigs.R, & ConvergeDiag.R

**Figures** Figures found in manuscript Folder with posterior estimates
from coefficients and key parameters

### Required Packages

nimble

auk

coda

ggplot2

devtools

dplyr

data.table

lubridate

bayesplot

cowplot

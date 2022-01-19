
#### Integrating community science and agency-collected monitoring data to expand monitoring capacity at large spatial scales

#### Contents of this repository

1.  Data Folder: Description text file with eBird data access query information and         links to download site covariate data for use in ArcMap. Washington Department of       Fish and Wildlife Common Loon observation data from 2017. SitesandCovs.csv data file     with all sites and covariate information, including indicators for data source          (eBird or WDFW). See DESCRIPTION.txt for more information and links to covariate        data sources.

2.  Scripts Folder: R scripts for filtering eBird data to map in ArcMap,
    formatting spatially filtered eBird data and WDFW data for use in
    the occupancy model, Nimble multi-state occupancy model script to run
    the model, and files to generate figures and parameter estimates

3.  Figures Folder:Figures 1-4 from the manuscript, folder with posterior coefficient       density plots from MCMC output

**Scripts**

1.  EBird_Auk_filtering.R & WDFWformat.R

2.  SPfilteredEBird.R & FormattedData.R

3.  NimbleOccModel.R

4.  PosteriorsandTables.R, PredictionsandFigs.R, & ConvergeDiag.R

**Figures** 

Figures found in manuscript Folder with posterior estimates
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

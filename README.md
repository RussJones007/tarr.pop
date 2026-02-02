## tarr.pop

A package for retrieving and managing population figures for epidemiological and public health analysis.

### Overview
tarr.pop simplifies the process of retrieving population figures for analysis. Originally developed for Tarrant County Public Health Epidemiology, it is designed to be flexible and can be used by other jurisdictions as well.
Population data is sourced from multiple authoritative providers and stored efficiently for large-scale analysis.

### Data Sources
The package integrates population data from the following sources:

 - Texas Demographic Center (TDC)
https://demographics.texas.gov/
Used for projections and estimates.


-  U.S. Census Bureau
Provides decennial census data, annual estimates, and ZCTA-level estimates.


-   Surveillance, Epidemiology, and End Results (SEER) Program
https://seer.cancer.gov/
Supplies county and census tract estimates.


#### Data Storage
Population data can be large, so it is stored in HDF5Arrays. Each source and data type (estimates, projections, census) is organized into individual HDF5Array files.
Standardized Dimensions
Arrays use standardized dimension names for consistency:

year
area.name — e.g., county names, census tract IDs
sex
age.char — age or age group
race
ethnicity

Where possible, dimension values are standardized (e.g., sex = "Male" or "Female"). However, age formats may vary: some arrays use single ages, others use age groups, or a mix of both.

#### Core Features
The package provides an S3 class tarr_pop to:

Wrap array details and provide array semantics
Enable filtering and collapsing of dimensions
Support typical array operations like indexing
Offer generic plotting via plot() and autoplot()

#### Dimension Operations

Filtering: Select specific dimension values
Collapsing: Combine dimension labels into larger groups

Example: Aggregate individual ages into age groups

Example: Combine geographic areas into larger units


#### Workflow for Retrieving Population Data:

* Open the chosen array (cube)
* Filter dimension names
* Collapse or summarize dimensions
* Convert to a data frame or save to disk


#### Example Usage
R
Load the package library(tarr.pop)
Open an arraypop_data <- open_tarr_pop("tdc_projection")
Filter by year and sex
filtered <- filter_tarr_pop(pop_data, year = 2025, sex = "Female")
Collapse age groups
collapsed <- collapse_tarr_pop(filtered, age.char = c("0-4", "5-9", "10-14"))# Convert to data framedf <- as.data.frame(collapsed)Show more lines

#### Why Use tarr.pop?

Handles large population datasets efficiently
Provides standardized dimensions for consistency
Flexible for epidemiology, public health, and demographic analysis
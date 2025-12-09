[![img](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

# Municipal Solid Waste Disposal in B.C.

A set of R scripts to create the municipal solid waste disposal in B.C. indicator published on [Environmental Reporting BC](https://www2.gov.bc.ca/gov/content?id=B71460AF7A8049D59F8CBA6EE18E93B8).

### Data

The data used for the indicator is available from the [B.C. Data Catalogue](https://catalogue.data.gov.bc.ca/dataset/d21ed158-0ac7-4afd-a03b-ce22df0096bc) under the [Open Government Licence - British Columbia](https://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61). Each year's new disposal rate data are provided by regional districts through the completion of the municipal solid waste disposal calculator run by the Environmental Standards Branch in the Ministry of Environment and Climate Change Strategy.

### Code

**For envreportbc team members that need to update the dataset:**  
There is one R script, `internal.R` which loads and combines the published 
[open data](https://catalogue.data.gov.bc.ca/dataset/bc-municipal-solid-waste-disposal-rates) 
with this years yet-to-be published values, and creates the output data files 
to be uploaded to the BC Data Catalogue and subsequently used in the indicator. 
This script requires input data from the Environmental Standards Branch. 

**For anyone else looking to generate the shinyapp, you do not need to run `internal.R`**

The dataviz folder contains code to create a Shiny app for an interactive 
data visualization used on the [indicator website](http://www.env.gov.bc.ca/soe/indicators/sustainability/municipal-solid-waste.html).

There is also one RMarkdown file associated with the indicator that generates the data visualizations and summary statistics.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an 
[Issue](https://github.com/bcgov/msw-disposal-indicator/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

    Copyright 2025 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.


This repository is maintained by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B). Click [here](https://github.com/bcgov/EnvReportBC) for a complete list of our repositories on GitHub.

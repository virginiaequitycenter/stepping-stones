# Updating Stepping Stones

This repo hosts the code to update and replicate the measures from the City of Charlottesville's [2019 Stepping Stones Report](https://www.charlottesville.gov/1623/Stepping-Stones-Report), produced by the Department of Human Services. The work focuses on over time trends of community and child well-being for the City of Charlottesville, Albemarle County, and the Commonwealth of Virginia.

The initial phase of this work to replicate nad extend the measures was completed by the [Public Interest Data: Ethics & Practice class](https://pidep23.mclaibourn.org/) in the Batten School of Leadership and Public Policy at the University of Virginia. The interim reports from the class can be viewed on the [class website](https://pidep23.mclaibourn.org/static/projects/).

The final phase of this work will be completed by the Democratization of Data team in [The Equity Center](https://virginiaequitycenter.org/) at UVA in collaboration with the City of Charlottesville's Department of Human Services.

## Code folder

The code folder contains scripts to acquire and prepare the approximately 40 measures used in the updated report. Where possible, data is pulled through the script, and captures data as far back as it is currently available online up to 1999. When the data could not be read in programmatically, the code contains comments describing how we generated and saved the original data files on which we rely. In a few instances, the initial data was manually curated (arrest data) or requested and received directly from the data owner (point-in-time counts of people experiencing homelessness from the Blue Ridge Area Coalition for the Homeless, the incidence of sexually transmitted diseases among youth from Virginia Department of Health).

In general the scripts

* Read in the data (from the original source or as downloaded by us)
* Process the data (filter, reshape, append locality and state totals, merge population data to generate percents and rates, etc.)
* Write out the data to be used in the report as a csv file.

## Data folder

The data folder contains all of the proccessed data as csv files. These are available for download and use by anyone (and many of the scripts could be revised to generate data for other localities). In general, the data file name matches the name of the script that generated it.

## Report folder

Currently, this contains the R markdown files that produce the interim reports from the class projects. 

## Other stuff

The classexamples folder contains a few files we used as the class was learning to work with the data. The docs folder contains the slides for a presentation about this work given to the City of Charlottesville's Data Analysis Group, focusing on how the class (and the Equity Center) sought to enact some key principles of equity in our process. It can be viewed [here](https://virginiaequitycenter.github.io/stepping-stones/dag_pres.html).

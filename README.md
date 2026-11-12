# DDES Converter & Annotation (Shiny application)

## Description

The `DDES Converter & Annotation` is an R/Shiny application for converting,
annotating, reviewing, and/or exporting digital polymerase chain reaction (dPCR)
data in the Digital PCR Data Essentials Standard (DDES) format.

The application guides the user through the following six steps:

1. Import demonstration data, or upload supported platform files.
2. Review and update the run metadata retrieved from the imported files.
3. Review, add, or remove probe and assay annotations.
4. Assign assays to wells, and/or update sample identifiers.
5. Review the generated main, assay, and intensity files.
6. Download the completed DDES archive.

## How to download and run locally

1. If not yet available, install R (https://cran.r-project.org/) and RStudio 
   Desktop (https://docs.posit.co/ide/user/download.html).
2. Click **Code**, then **Download zip** to download the repository (or clone it 
   using Git). Extract the downloaded zip file.
3. Open RStudio and, if not done previously, install the required packages for
   `DDES-Shiny`:

```r
install.packages(
  c(
    "shiny",
    "bslib",
    "rhandsontable",
    "stringr",
    "zip",
    "reshape2",
    "data.table",
    "shinyFeedback",
    "shinyalert"
  )
)
```

4. In RStudio, open `ui.R`, `server.R`, or `global.R` from the `DDES-Shiny`
   directory and select **Run App**. Alternatively, you can set the 
   application's directory and run the app as follows:

```r
setwd("C:/Users/username/Documents/DDES-Shiny")
shiny::runApp()
```

(Replace the example directory with the location of `DDES-Shiny` on your 
computer). 

5. The application opens in RStudio, or a web browser and can be used.

## Version history

### Version 1.0.0

- Initial release of the Shiny conversion and annotation workflow.
- Support for QIAGEN QIAcuity, and Bio-Rad QX200 QuantaSoft exports.

## Supported input data formats in the latest version

### QIAGEN QIAcuity

The application requires:

- An analysis results file (comma-separated values `.csv` format).
- A plate-layout file (`.csv` format).
- A relative fluorescence unit (RFU) file (`.zip` format).

### Bio-Rad QX200 with QuantaSoft

The application requires:

- A head file (`.csv` format).
- Well amplitude files (`.csv` format).

### Other data formats

We welcome pull requests to add support for additional data formats. The DDES 
consortium is actively working with platform providers to provide DDES 
functionality either in this application, and/or as part of their software.

## DDES output structure

The application produces one `.ddes` file, containing:

**Main file:** Run metadata and the sample, assay, target, count,
  concentration, and threshold information for each reaction.
**Assay file:** Assay definitions linking each assay to its target and
  fluorescence-channel combinations.
**Intensity file(s):** One file per well containing the partition identifier,
  fluorescence channel, and endpoint fluorescence intensity for every
  partition.
  



## ROFSim
SyncroSim prototype package for integrating SpaDES, LandR and FireSense projections with Caribou Habitat RSF models in the "Caribou Metrics" package.

## Installation and User quick guide

### Step 1: Obtain all required software
1. Download and install version 2.2.41 of Syncrosim.
2. Download and install the latest version of R (4.0.5) [here](https://www.r-project.org/) (note that any version above or equal to 4.0.0 should work, so if you already have 4.0.0 you do not need to update your to the latest minor update). 
3. (*Optional but highly recommended*) Download and install RStudio [here](https://www.rstudio.com/products/rstudio/download/) (note that any version above or equal to 1.3 should work).
4. Open R, either with the command line or simply by opening RStudio.
5. Install the required packages. Note that the `caribouMetrics` package is hosted on a [private GitHub repository](https://github.com/LandSciTech/caribouMetrics) (and not on CRAN like most packages) and therefore cannot be installed with `install.packages()` and requires the package `devtools` and its `install_github` function.

```r
install.packages(c("SpaDES", "SpaDES.core", "SpaDES.tools", "data.table", "qs", "devtools"))  
devtools::install_github("https://github.com/LandSciTech/caribouMetrics")
```

### Step 2: Obtain and organize all data

1. Download the initial conditions files.

In addition, if you are interested in using SpaDES files: 

2. Download at least two replicates of the LandR/FireSense simulation results (TODO add open link).
3. Extract each of the archives into a separate folder. On Windows you might need to download software to extract the files, for example [7-Zip](https://www.7-zip.org/). Also note that you might have to extract twice (once to get the `.tar` file out of the archive and once to get the rest of the files).
4. In each of the extracted folders, identify the `.qs` file that bear the name of the original arcghive. These will be the "SpaDES files" that will be needed. 

### Step 3: Set up the ROFSim library

1. Create a new Library and load the main Project-level datasheets: Caribou Ranges, Seasons, and parameterize the inputs of the model.
2. If interested in the SpaDES results, run the SpaDES import and MakeLCCFromSpaDES transformer. If onluy interested in the caribouMetrics model, run the Run Caribou Model transformer.


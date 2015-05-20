# extrapolation
extrapolaton factors probabilistic application to BMD

## Installation

There are three alternative ways of installing (some may fail according to your personal set-up, just find the on which works for you), in any case
dependencies has to be installed first.

### Installing dependencies

`install.packages("logging")`  
`install.packages("data.table")`

### Installation method 1 (straight from GitHub)
`install.packages("https://github.com/azabeo/extrapolation/archive/v0.1.tar.gz", repo = NULL, type = 'source')`

### Installation method 2 (From my personal repository)
`install.packages("extrapolation", repos = "http://www.dsi.unive.it/~zabeo/R/" )`

### Installation method 3 (From downloaded package)
Download the most updated `tar.gz` package from the [release](https://github.com/azabeo/extrapolation/releases) tab, move it to your R workspace folder, then from R type:  
`install.packages("extrapolation-0.1.tar.gz", repo = NULL, type = 'source')`

## Usage

The software reads its inputs directly from standard csv files (with headers and comma as separator). Inputs are two tables, one for bmds and one for Extrapolaiton Factors to be applied. The specifics of input tables' formats are reported below. To store results into a `res` variable:  
`res <- extrapolation(bmd.file.name = "example.data/bmd.csv", efs.file.name = "example.data/Efs.csv", above.threshold = 0)`

### Arguments

* *bmd.file.name*	string. the location of the input file for bmds ([see specification below](#bmds))
* *efs.file.name*	string. table of EFs to be applied ([see specification below](#efs))
* *above.threshold*	numeric. If present values less equal to it are discarded from generated bmd distribution

### Returns

List of two tables, the updated bmds table with results and the EFs table used

### <a name="bmds"></a> bmds table specification

| id            | bmd       |   |   |   |
|---------------|-----------|---|---|---|
| id of the row | bmd value |   |   |   |

### <a name="efs"></a>EFs table specification

| name           | mu                         | sigma                       | dist.type                                     | is.geom                                                                                       | operation                          | above.threshold                                              |
|----------------|----------------------------|-----------------------------|-----------------------------------------------|-----------------------------------------------------------------------------------------------|------------------------------------|--------------------------------------------------------------|
| name of the EF | means GM if "geom" is TRUE | means GSD if "geom" is TRUE | can be "rnorm"(normal) or "rlnorm"(lognormal) | if "dist.type" is "lognormal" and "is.geom" is TRUE "mu" and "sigma" are trated as GM and GSD | either "div", "mult", "sum", "sub" | if present, values <= of the "above.threshold" are discarded |

## Data

* The software works directly with csv tables, exemplificative tables can be found in the `example.data` folder.
* The example data can also be directly loaded in variables to look at them from R by
    * `bmds <- readRDS("data/bmds.rds")` 
    * `efs <- readRDS("data/efs.rds")`

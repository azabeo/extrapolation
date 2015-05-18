# extrapolation
extrapolaton factors probabilistic application to BMD

## Installation

`install.packages("https://github.com/azabeo/extrapolation/archive/v0.1.tar.gz", repo = NULL, type = 'source')`

## Usage

`res <- extrapolation(bmd.file.name = "example.data/bmd.csv", efs.file.name = "example.data/Efs.csv", above.threshold = 0)`

## Arguments

* *bmd.file.name*	string. the location of the input file for bmds (al least it has to have 'bmd' and 'id' columns)
* *efs.file.name*	string. table of EFs to be applied (see excel file in inst for instructions)
* *above.threshold*	numeric. If present values less equal to it are discarded from generated bmd distribution

## Returns

List of two tables, the updated bmds table with results and the EFs table used

## Data

* Example csv and excel files in `example.data` folder
* Example data can be directly loaded in the workspace by `bmds <- readRDS("data/bmds.rds")` and `efs <- readRDS("data/efs.rds")`

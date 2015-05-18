# extrapolation
extrapolaton factors probabilistic application to BMD

## Usage

`extrapolation(bmd.file.name = "inst/bmd.csv", efs.file.name = "inst/Efs.csv", above.threshold = 0)`

## Arguments

* *bmd.file.name*	string. the location of the input file for bmds (al least it has to have 'bmd' and 'id' columns)
* *efs.file.name*	string. table of EFs to be applied (see excel file in inst for instructions)
* *above.threshold*	numeric. If present values less equal to it are discarded from generated bmd distribution

# HAVPlot data compilation

Compile raw HAVPlot data from separate CSV files into a single table.

### Package dependencies
- tidyverse
- APCalign

## Usage

Run `main.R` to execute the full pipeline.

## Data

HAVPlot dataset is available [here](https://researchdata.edu.au/harmonised-australian-vegetation-dataset-havplot/1950860)

Further information including details of HAVPlot data structure is [here](https://data.csiro.au/collection/csiro:54461?_st=browse&_str=2&_si=1&browseType=kw&browseValue=plot)

## Output

The output is a `data.frame` which compiles selected columns from the raw HAVPlot tables.

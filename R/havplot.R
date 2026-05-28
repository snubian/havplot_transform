
# Dependencies: tidyverse, APCalign

# Loads HAVPlot data from a given path.
# Specify a path only, no filename required, end slash optional.
# Returns a list with six data.frame elements corresponding to the six HAVPlot CSV files.
# e.g.
#   d <- load_havplot("mydata/havplot/)
load_havplot <- function(path) {
  
  list(
    project                        = read_csv(sprintf("%s/project.csv", path)),
    
    # Specify explicit col_types for a couple of columns that read_csv interprets
    # incorrectly as numeric.
    # slopeAspect is generally numeric but also includes directions such as "SW"
    # slopeGradient should probably be numeric always but has one value of "5%"
    # which it is not necessary to correct at this stage.
    # Currently don't include these fields in the compiled output.
    plot                           = read_csv(sprintf("%s/plot.csv", path),
                                              col_types = cols(
                                                slopeAspect = col_character(),
                                                slopeGradient = col_character()
                                              )
                                     ),
    plot_observation               = read_csv(sprintf("%s/plotObservation.csv", path)),
    aggregate_organism_observation = read_csv(sprintf("%s/aggregateOrganismObservation.csv", path)),
    aggregate_soil_observation     = read_csv(sprintf("%s/aggregatesoilObservation.csv", path)),
    species_attributes             = read_csv(sprintf("%s/speciesAttributes.csv", path))
  )
  
}

# Make a small number of corrections to obvious errors in raw HAVPlot data, which may in some cases
# cause problems later on when joining tables.
# Returns list as passed, with corrections made.
correct_havplot <- function(data) {

  # These are obvious errors in taxon names that need to be corrected to allow
  # smooth table joins.
  data$aggregate_organism_observation <-
    data$aggregate_organism_observation %>%
    mutate(
      scientificName = ifelse(scientificName == "Blechnum sp. sp.", "Blechnum sp.", scientificName),
      scientificName = ifelse(scientificName == "Cardamine tenuifolia sp.", "Cardamine tenuifolia", scientificName)
    )
  
  # There are a few duplicate taxon names in the species_attributes table which causes trouble
  # when joining. The duplicates differ only by AusNativeStatus, so remove the incorrect
  # dupes.
  data$species_attributes <-
    data$species_attributes %>%
    filter(
      !(scientificName == "Epipremnum pinnatum" & AusNativeStatus == "non-native"),
      !(scientificName == "Solidago altissima" & AusNativeStatus == "native"),
      !(scientificName == "Tristemma mauritianum" & AusNativeStatus == "native")
    ) %>%
    distinct()

  data$project <-
    data$project %>%
    mutate(
      
      # There are probably many errors such as this in the dataset, where data from one column has
      # been shifted sideways into an adjacent column at some point. This one caught my eye so I
      # fix it here (researcher's name is in `abstract` rather than `individualName`)
      individualName = ifelse(individualName == "University of Adelaide", abstract, individualName)
    )
  
  data
  
}

# Join separate HAVPlot tables from list of data.frames into a single data.frame.
compile_havplot <- function(data) {
  
  # The principle here is to begin with the table containing taxon names
  # and join tables using the IDs as defined in the HAVPlot:
  # - begin with aggregate_organism_observation
  # - join to species_attributes using `scientificName`
  # - join to plot_observation using `plotObservationID`
  # - join to plot using `plotID`
  # - join to project using `projectID`
  
  # Along the way we select only relevant columns from each table.
  # Note that the HAVPlot table aggregate_soil_observation is not used at present.
  data$aggregate_organism_observation %>%
    filter(
      
      # Remove records having unknown taxon name
      !is.na(scientificName),
      scientificName != "unidentified"
    ) %>%
    dplyr::select(
      -verbatimScientificName
    ) %>%

    left_join(data$species_attributes, by = "scientificName") %>%

    left_join(data$plot_observation %>% dplyr::select(plotID:obsStartDate), by = "plotObservationID") %>%

    left_join(
      data$plot %>%
        dplyr::select(
          plotID,
          decimalLongitude:geodeticDatum,
          area
        ),
      by = "plotID"
    ) %>%

    left_join(
      data$project %>%
        dplyr::select(
          projectID,
          organizationName,
          individualName,
          projectStartDate,
          abundanceMethod,
          HAVPlotDataSource,
          sourceDataLicense
        ),
      by = "projectID"
    )
  
}

# Around 95% of taxon names in HAVPlot match to accepted names as per the APC.
# For the rest we do a lookup using APCalign and join the results to the 
# compiled HAVPlot table.
# The parameter apc refers to a data structure as returned from a call to
# APCalign::load_taxonomic_resources()
align_taxonomy_havplot <- function(data, apc) {
  
  apc_accepted <- apc$`APC list (accepted)`

  species_not_accepted <-
    setdiff(
      unique(data[data$taxonRank == "species", ]$scientificName),
      apc_accepted$canonical_name
    )
  
  species_aligned <- APCalign::create_taxonomic_update_lookup(species_not_accepted, resources = apc)
  
  data %>%
    left_join(
      species_aligned %>%
        dplyr::select(
          original_name,
          suggested_name,
          taxonomic_dataset,
          taxonomic_status,
          update_reason
        ),
      by = c("scientificName" = "original_name")
    )
  
}

# Specify column order and sorting of the final compiled HAVPlot output.
arrange_havplot <- function (data) {
  
  data %>%
    dplyr::select(
      projectID,
      organizationName,
      individualName,
      projectStartDate,
      HAVPlotDataSource,
      sourceDataLicense,
      plotID,
      decimalLongitude,
      decimalLatitude,
      geodeticDatum,
      area,
      plotObservationID,
      obsStartDate,
      scientificName,
      taxonRank,
      family,
      genus,
      AusNativeStatus,
      abundanceValue,
      abundanceUnits,
      abundanceMethod,
      suggested_name,
      taxonomic_dataset,
      taxonomic_status,
      update_reason
    ) %>%
    arrange(
      projectID,
      plotID,
      plotObservationID,
      scientificName
    )
  
}

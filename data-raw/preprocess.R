# Load raw data from .sav file
exampleData <- read_sav("data-raw/Lernkultur_Zusammengefügt.sav")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(exampleData)
# Prepare data, write CSV data tables

# Before: SWO_age_data_for_SPC_200421.csv (boot/data)
# After:  otoliths.csv (data)

library(TAF)

mkdir("data")

# Read otoliths
otoliths <- read.taf("boot/data/SWO_age_data_for_SPC_200421.csv")

# Format table
names(otoliths) <- c("id", "length", "sex", "age", "readability")
otoliths$sex <- tolower(otoliths$sex)
otoliths <- otoliths[c("age", "length", "sex")]
otoliths <- otoliths[order(otoliths$age),]

# Convert eye orbital fork length (EOFL) measurements
# to lower jaw fork length (LJFL) used in the assessment
# Conversion factor taken from SWO 2021 script by Nicholas
# (vb_growth_2sex_fixedEffects.r)
otoliths$length <- 1.1111 * otoliths$length

# Write table
write.taf(otoliths, dir="data")

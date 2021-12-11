# Code for *Distribution Grid Impacts of Electric Vehicles: A California Case Study*

The following files pertain to code used to parse data and conduct analysis for the publication titled "Distribution Grid Impacts of Electric Vehicles: A California Case Study" in *iScience*.  Brief descriptions of the code files are provided below.  Please direct any additional questions or requests to Alan Jenn (ajenn@ucdavis.edu).

**buffer_stuff.R** - This script produces a key table that matches census blocks to the nearest distribution feeder within PG&E's network.

**load_evmt.R** - This script loads and merges eVMT data from UC Davis' PHEV Center.

**process_cvrp.R** - This script loads and parses the Clean Vehicle Rebate Program's electric vehicle purchasing data to determine trends used in other parts of the analysis.

**load_profiles_clean.R** - This script generates daily electric vehicle load profiles based on charging data from UC Davis' PHEV Center's eVMT project.  The script also produces baseload profiles for each utility feeder in PG&E.

**combine_models.R** - This script conducts the aggregate analysis across each individual feeder within PG&E's service territory.  The model combines all of the previous data parsing elements to produce final results matching loads to feeders.

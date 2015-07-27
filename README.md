# REDATAMtoR
This repo contains functions to convert SYLK exported files from cepal's REDATAM to a R data frame

## retadamFrequenciesToR

This function takes a frequencies output of REDATAM with *RADIOS* as AREABREAKS (exportes as a csv file from the original SYLK file) and returns a data.frame objet within the R enviroment. It does not saves the output as a csv, rda o rds file. The *csv* file must have as separator a semicolon (*;*) for the function to work. Also, it must be only *FREQUENCIES*, it does not work with *CORSSTABS*. 

The functiones takes two parameters **archivo**, the path to the csv file, and **chequeo**, a boolean where TRUE means it first runs a subrutine to check wehter the sum of the RADIOS's totals adds up to the full totals given by the REDATAM original file under the *RESUMEN* title.   

# Future work

- Translate the function's comments 
- Improve the **chequeo** parameter 
- Develop another function that takes *CROSSTABS* and another types of REDATAM's outputs

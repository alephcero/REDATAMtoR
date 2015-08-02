library(reshape)
# This function takes an ASCII redatam exported text file with averages of a variable with RADIOS as areabreaks
# and returns an R data.frame object

redatamAverages = function(archivo) {
  
  #Read files in ASCII format
  data = readLines(archivo)
  data = iconv(data, "latin1", "utf-8")
  
  #Removes any empty tables
  posicionTablasVacias = grep("Tabla",data)
  data = data[-(posicionTablasVacias-2)]
  
  #Creates a data.frame with the areas
  posicionAreas = grep("AREA",data)
  areas = data[posicionAreas]
  areas = gsub(" ","",areas)
  areas = as.data.frame(areas)
  areas = colsplit(areas$areas,"\t",c("vacio1","area"))
  areas = as.data.frame(areas[,2])
  names(areas)="areas"
  
  areas$LINK = substr(areas$areas,6,14)
  areas$comuna = as.numeric(substr(areas$areas,8,10))
  areas$fraccion = as.numeric(substr(areas$areas,11,12))
  areas$radio = as.numeric(substr(areas$areas,13,14))
  areas$CO_FRAC_RA = paste(areas$comuna,areas$fraccion,areas$radio,sep="_")
  
  
  
  #Gets the data set for the variable ante the amount of households
  posicionDatos = grep("Total y Promedio",data)
  datos = data[posicionDatos]
  datos = gsub("\t100,00 %","",datos)
  datos = gsub("\t Total y Promedio\t","",datos)
  datos = gsub(",",".",datos)
  
  #separates the data set with resumes the total of households
  datosResumen = datos[length(datos)]
  datosResumen = as.data.frame(datosResumen)
  datos = as.data.frame(datos[-length(datos)])
  
  datos = colsplit(datos$datos,"\t",c("cantidad","capeco"))
  datosResumen = colsplit(datosResumen$datos,"\t",c("cantidad","capeco"))
  datosResumen$cantidad = as.character(datosResumen$cantidad)
  datosResumen$cantidad = gsub(".","",datosResumen$cantidad,fixed = TRUE)
  
  #check for the sum of households in the table adds up to the resume table
  check1 = sum(datos$cantidad) == datosResumen$cantidad
  #checks that both tables have the same amount of rows
  check2 = nrow(areas)==nrow(datos)
  
  if(check1 & check2){
    base = cbind(areas,datos)
    base
  } else {
    stop("ATENCIÓN: La suma de los radios NO coincide con los totales en la tabla resumen o no son la misma cantidad de areas y datos")
  }
}



#Cargar librerias
leerTablasRedatam = function(archivo,chequeo=FALSE) {
  #Esta funcion toma un archivo de frecuencias por radios en excel, lo ordena en un data.frame.
  #El archivo debe ser una ruta a un csv separado por ;
  #No sirve si hay cruces 
  
  #ESTAN FAYANDO LOS NUMEROS DE LA FILA ETC
  #Ver de integrar en el script de R algun marcador de inicio
  
  #Leer archivos
  #archivo = "bases/Censo/exportacionesRedatamOnline/calmat/calmatNuevo.csv"
  #archivo = "bases/Censo/exportacionesRedatamOnline/CAPECO/capecoCABAnuevo.csv"
  #archivo = "bases/Censo/exportacionesRedatamOnline/regimenTenencia/regimenTenenciaNuevo.csv"
  
  data = read.csv(file=archivo,
                  sep=";")
  
  
  #nombreDeLaBase = as.character(data[1,1])
  
  #Detecar comienzo y final 
  #El comienzo es con AREA y tiene dos reglones abajo
  #filaComienzaConAREA = grep("AREA",data[,1])
  #data = data[filaComienzaConAREA[1]:nrow(data)-2,] 
  
  
  #Elimino la columna de porcentajes acumulados
  data = data[,1:3]
  
  
  #Tabla de validacion 
  resumen = grep("RESUMEN",data[,1])
  validacion = data[(resumen+2):(nrow(data)),]
  names(validacion)=c(as.character(validacion[1,1]),
                      as.character(validacion[1,2]),
                      as.character(validacion[1,3]))
  validacion = validacion[2:nrow(validacion),]
  
  finValidacion = grep("Total",validacion[,1])
  validacion = validacion[1:finValidacion,]
  rm(finValidacion)
  
  validacion[,2] =  as.integer(as.character(validacion[,2]))
  validacion[,3] =  as.integer(as.character(validacion[,3]))
  
  #Tomo las categorias de la variable
  categoriasVariable = as.character(validacion[,1])
  categoriasVariable = categoriasVariable[1:(length(categoriasVariable)-1)]
  
  
  #Elimino la tabla final resumen de la data
  data = data[1:(resumen-1),]
  finData = grep("Total",data[,1])
  finData = finData[length(finData)]
  data = data[1:finData,]
  #rm(finData,resumen)
  
  
  #Detectar las tablas vacias y borrarlas, haciendo que la tabla siguiente ocupe su lugar exacto
  tablasVacias = grep("Tabla",data[,1]) #La tabla vacia se indica con ese texto
  #data = data[-(tablasVacias:(tablasVacias-1)),]
  
  
  
  #Creo la base vacía contra la cual hacer el join
  base = data.frame(categorias = c(categoriasVariable))
  base$categorias = as.character(base$categorias)
  
  #Creo el listado de radios que hay en el csv
  comienzaConAREA = grep("AREA",data[,1])
  AREAvacia = tablasVacias - 2
  AREASconDato = comienzaConAREA[!comienzaConAREA%in%AREAvacia]
  listadoRadios = as.character(data[AREASconDato,1])
  listadoRadios = gsub(" ", "", listadoRadios, fixed = TRUE)
  #rm(comienzaConAREA,AREASconDato,AREASconDato)
  
  
  #Detecto el comienzo y el fin de cada tabla de datos para cada radio
  comienzoDelDF = grep("Casos",data[,2]) #El dataframe siempre arranca con el encabezado de cada tabla. La 4 columna tiene el "Total"
  comienzoDelDF = comienzoDelDF + 1 #Le sumo 1 porque comienza en el encabezado y solo quiero la data posta
  finDelDF = grep("Total",data[,1]) #Siempre termina en la primer columna con el total
  finDelDF = finDelDF - 1 #no me interesa el total 
  
  
  if (chequeo) {
    for (i in 1:length(listadoRadios)) {
      filaInicio = comienzoDelDF[i]
      filaFin = finDelDF[i]
      #Elimino la columna de totales
      df = data[filaInicio:filaFin,c(1,2)] #Esta sirve para hacer el chequeo porque despues puedo contar la sumatoria
      names(df) = c("categorias","porcentaje")
      df$categorias = as.character(df$categorias)
      df$porcentaje = as.numeric(as.character(df$porcentaje))
      base = left_join(base,df)
      base$porcentaje[is.na(base$porcentaje)] = 0
      names(base)[i+1] = paste(listadoRadios[i])
    }
    #La suma de las filas debe dar igual que la tabla de validación
    sonIdenticas = identical(as.numeric(rowSums(base[,2:ncol(base)])),as.numeric(validacion$Casos[1:(nrow(validacion)-1)]))
    
    if (sonIdenticas) {
      print("La suma de los radios coincide con los totales en la tabla resumen")
    } else {
      print("ATENCIÓN: La suma de los radios NO coincide con los totales en la tabla resumen")
      }
    
    #se vuelve a limpiar la base
    base = data.frame(categorias = c(categoriasVariable))
    base$categorias = as.character(base$categorias)
  }
  
  
  
  for (i in 1:length(listadoRadios)) {
    filaInicio = comienzoDelDF[i]
    filaFin = finDelDF[i]
    #Elimino la columna de totales
    df = data[filaInicio:filaFin,c(1,3)]
    names(df) = c("categorias","porcentaje")
    df$categorias = as.character(df$categorias)
    df$porcentaje = as.numeric(as.character(df$porcentaje))
    base = left_join(base,df)
    base$porcentaje[is.na(base$porcentaje)] = 0
    names(base)[i+1] = paste(listadoRadios[i])
  }
  #rm(data,df,AREASconDato,AREAvacia,categoriasVariable,comienzoDelDF,comienzaConAREA,filaFin,filaInicio,finDelDF,i,listadoRadios,tablasVacias)
  

  nombresColumna = as.character(base$categorias)
  
  
  radios = data.frame(radios = names(base)[2:ncol(base)])
  radios$LINK = substr(radios$radios,6,14)
  radios$comuna = as.numeric(substr(radios$radios,8,10))
  radios$fraccion = as.numeric(substr(radios$radios,11,12))
  radios$radio = as.numeric(substr(radios$radios,13,14))
  radios$CO_FRAC_RA = paste(radios$comuna,radios$fraccion,radios$radio,sep="_")

  base = base[,-1]
  base = t(base)
  
  base = as.data.frame(base)
  names(base)=nombresColumna
  
  
  base = cbind(radios,base)
  base
  #rm(radios,nombresColumna)
}

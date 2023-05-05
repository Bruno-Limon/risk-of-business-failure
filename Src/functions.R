library(dplyr)

#get geographical area
getGeoArea<-function(data) {
  if ("GeoArea" %in% colnames(data)) 
    return (data)
  # South = Abruzzo, Basilicata, Calabria, Campania, Molise, Puglia, Sardegna and Sicilia
  data$GeoArea=factor("South", levels = c("South", "Centre", "North"))
  # North = Liguria, Lombardia, Piemonte, Valle d'Aosta, Emilia-Romagna, Friuli-Venezia Giulia, Trentino-Alto Adige and Veneto
  data$GeoArea[data$`Registered office address - Region`=="Liguria" 
               | data$`Registered office address - Region`=="Lombardia" 
               | data$`Registered office address - Region`=="Piemonte" 
               | data$`Registered office address - Region`=="Valle d'Aosta/Vallée d'Aoste" 
               | data$`Registered office address - Region`=="Emilia-Romagna"
               | data$`Registered office address - Region`=="Friuli-Venezia Giulia"
               | data$`Registered office address - Region`=="Trentino-Alto Adige"
               | data$`Registered office address - Region`=="Veneto" ]= "North"
  # Center = Lazio, Marche, Toscana and Umbria
  data$GeoArea[data$`Registered office address - Region`=="Lazio" 
               | data$`Registered office address - Region`=="Marche" 
               | data$`Registered office address - Region`=="Toscana" 
               | data$`Registered office address - Region`=="Umbria"  ]= "Centre"
  
  data$GeoArea = as.factor(data$GeoArea)
  return(data)
}

# get company size
getSize <- function(data) {
  if ("Size" %in% colnames(data)) 
    return (data)

  data <- data[!is.na(data$`Total assetsth EURLast avail. yr`),]
  data$Size <- log(data$`Total assetsth EURLast avail. yr`)
  #data$Size[data$`Total assetsth EURLast avail. yr`== 0] <- 0 
  data$Size <- scales::rescale(data$Size, to = c(0,10))
  
#  data$Size <- factor("Medium", levels = c("Medium", "Small", "Large"))
#  data$Size[data$`Number of employeesLast avail. yr` <= 50] <- "Small"
#  data$Size[data$`Number of employeesLast avail. yr` >= 250] <- "Large"
  
  return(data)
}

#get size class
getSizeClass <- function(data) {
  data$SizeClass <- factor("Medium", levels = c("Medium", "Small", "Large"))
  data$SizeClass[data$`Number of employeesLast avail. yr` <= 50] <- "Small"
  data$SizeClass[data$`Number of employeesLast avail. yr` >= 250] <- "Large"
  
  data$SizeClass <- as.factor(data$SizeClass)
  return(data)
}

# get liquidity
getLiquidity <- function(data) {
  if ("LiqudiityRatio" %in% colnames(data)) 
    return (data)
  
  data <- data[!is.na(data$`Liquidity ratioLast avail. yr`),]
  data$LiquidityRatio <- data$`Liquidity ratioLast avail. yr`

  return(data)
}

# get active or failed status
getStatus <- function(data) {
  if ("Status" %in% colnames(data)) 
    return (data)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  data$Status <- NULL
  actives <- c('Active','Active (default of payments)','Active (receivership)')
  # a company is considered active if belonging to any of the variations of active, failed otherwise
  data[data$`Legal status` %in% actives,'Status'] <- 'Active'
  data[data$`Legal status` %!in% actives,'Status'] <- 'Failed'
  data$Status <- as.factor(data$Status)
  
  return(data)
}

# get age
getAge <- function(data) {
  if ("Age" %in% colnames(data)) 
    return (data)
  # removing companies without incorporation year
  data <- data[!is.na(data$`Incorporation year`),]
  # we consider age as the difference between last accounting closing date and incorporation year
  data$Age <- data$`Last accounting closing date` - data$`Incorporation year`
  data$Age <- as.integer(data$Age)
  data <- data[data$Age >= 0,]
  
  return(data)
}

# create 3 sub-datasets, one for each year
getCurrYr <- function(data) {
  keepCurrYr <- c("Year - 1", "Year - 2")
  data <- select(data, -contains(keepCurrYr))
  
  return(data)
}
getYr1 <- function(data) {
  keepYr1 <- c("yr", "Year - 2")
  data <- select(data, -contains(keepYr1))
  
  return(data)
}
getYr2 <- function(data) {
  keepYr2 <- c("yr", "Year - 1")
  data <- select(data, -contains(keepYr2))
  
  return(data)
}

# get ATECTO sectors
getATECO <- function(data) {
  if ("ATECO.Sector.Name" %in% colnames(data)) 
    return (data)
  # from http://www.fr.camcom.gov.it/sites/default/files/cciaa/RinnovoConsiglio/ateco-2007-struttura.pdf
  # based on the previous document, we focus on the first 2 digits of the ATECTO 2007 code, in order to map it to its specific sector letter, going from A to U
  
  # data <- data[!is.na(data$`ATECO 2007code`),]
  data$ATECO.Sector.Code <- NULL
  data$ATECO.Sector.Code <- substr(data$`ATECO 2007code`, 0, 2)
  
  # making a dataframe with sector code, name and description
  ATECO.Sector.Code <- c('01', '02', '03', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '35', '36', '37', '38', '39', '41', '42', '43', '45', '46', '47', '49', '50', '51', '52', '53', '55', '56', '58', '59', '60', '61', '62', '63', '64', '65', '66', '68', '69', '70', '71', '72', '73', '74', '75', '77', '78', '79', '80', '81', '82', '84', '85', '86', '87', '88', '90', '91', '92', '93', '94', '95', '96', '97', '98', '99', '00')
  
  ATECO.Sector.Name <- c('A', 'A', 'A', 'B', 'B', 'B', 'B', 'B', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'D', 'E', 'E', 'E', 'E', 'F', 'F', 'F', 'G', 'G', 'G', 'H', 'H', 'H', 'H', 'H', 'I', 'I', 'J', 'J', 'J', 'J', 'J', 'J', 'K', 'K', 'K', 'L', 'M', 'M', 'M', 'M', 'M', 'M', 'M', 'N', 'N', 'N', 'N', 'N', 'N', 'O', 'P', 'Q', 'Q', 'Q', 'R', 'R', 'R', 'R', 'S', 'S', 'S', 'T', 'T','U', 'NA')
  
  ATECO.Sector.Description <- c('AGRICOLTURA, SILVICOLTURA E PESCA', 'AGRICOLTURA, SILVICOLTURA E PESCA', 'AGRICOLTURA, SILVICOLTURA E PESCA','ESTRAZIONE DI MINERALI DA CAVE E MINIERE', 'ESTRAZIONE DI MINERALI DA CAVE E MINIERE', 'ESTRAZIONE DI MINERALI DA CAVE E MINIERE', 'ESTRAZIONE DI MINERALI DA CAVE E MINIERE', 'ESTRAZIONE DI MINERALI DA CAVE E MINIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'ATTIVITÀ MANIFATTURIERE', 'FORNITURA DI ENERGIA ELETTRICA, GAS, VAPORE E ARIA CONDIZIONATA', 'FORNITURA DI ACQUA; RETI FOGNARIE, ATTIVITÀ DI GESTIONE DEI RIFIUTI E RISANAMENTO', 'FORNITURA DI ACQUA; RETI FOGNARIE, ATTIVITÀ DI GESTIONE DEI RIFIUTI E RISANAMENTO', 'FORNITURA DI ACQUA; RETI FOGNARIE, ATTIVITÀ DI GESTIONE DEI RIFIUTI E RISANAMENTO', 'FORNITURA DI ACQUA; RETI FOGNARIE, ATTIVITÀ DI GESTIONE DEI RIFIUTI E RISANAMENTO', 'COSTRUZIONI', 'COSTRUZIONI', 'COSTRUZIONI', 'COMMERCIO ALL INGROSSO E AL DETTAGLIO; RIPARAZIONE DI AUTOVEICOLI E MOTOCICLI', 'COMMERCIO ALL INGROSSO E AL DETTAGLIO; RIPARAZIONE DI AUTOVEICOLI E MOTOCICLI', 'COMMERCIO ALL INGROSSO E AL DETTAGLIO; RIPARAZIONE DI AUTOVEICOLI E MOTOCICLI', 'TRASPORTO E MAGAZZINAGGIO', 'TRASPORTO E MAGAZZINAGGIO', 'TRASPORTO E MAGAZZINAGGIO', 'TRASPORTO E MAGAZZINAGGIO', 'TRASPORTO E MAGAZZINAGGIO', 'ATTIVITÀ DEI SERVIZI DI ALLOGGIO E DI RISTORAZIONE', 'ATTIVITÀ DEI SERVIZI DI ALLOGGIO E DI RISTORAZIONE', 'SERVIZI DI INFORMAZIONE E COMUNICAZIONE', 'SERVIZI DI INFORMAZIONE E COMUNICAZIONE', 'SERVIZI DI INFORMAZIONE E COMUNICAZIONE', 'SERVIZI DI INFORMAZIONE E COMUNICAZIONE', 'SERVIZI DI INFORMAZIONE E COMUNICAZIONE', 'SERVIZI DI INFORMAZIONE E COMUNICAZIONE', 'ATTIVITÀ FINANZIARIE E ASSICURATIVE', 'ATTIVITÀ FINANZIARIE E ASSICURATIVE', 'ATTIVITÀ FINANZIARIE E ASSICURATIVE', 'ATTIVITÀ IMMOBILIARI', 'ATTIVITÀ PROFESSIONALI, SCIENTIFICHE E TECNICHE', 'ATTIVITÀ PROFESSIONALI, SCIENTIFICHE E TECNICHE', 'ATTIVITÀ PROFESSIONALI, SCIENTIFICHE E TECNICHE', 'ATTIVITÀ PROFESSIONALI, SCIENTIFICHE E TECNICHE', 'ATTIVITÀ PROFESSIONALI, SCIENTIFICHE E TECNICHE', 'ATTIVITÀ PROFESSIONALI, SCIENTIFICHE E TECNICHE', 'ATTIVITÀ PROFESSIONALI, SCIENTIFICHE E TECNICHE', 'NOLEGGIO, AGENZIE DI VIAGGIO, SERVIZI DI SUPPORTO ALLE IMPRESE', 'NOLEGGIO, AGENZIE DI VIAGGIO, SERVIZI DI SUPPORTO ALLE IMPRESE', 'NOLEGGIO, AGENZIE DI VIAGGIO, SERVIZI DI SUPPORTO ALLE IMPRESE', 'NOLEGGIO, AGENZIE DI VIAGGIO, SERVIZI DI SUPPORTO ALLE IMPRESE', 'NOLEGGIO, AGENZIE DI VIAGGIO, SERVIZI DI SUPPORTO ALLE IMPRESE', 'NOLEGGIO, AGENZIE DI VIAGGIO, SERVIZI DI SUPPORTO ALLE IMPRESE', 'AMMINISTRAZIONE PUBBLICA E DIFESA; ASSICURAZIONE SOCIALE OBBLIGATORIA', 'ISTRUZIONE', 'SANITÀ E ASSISTENZA SOCIALE', 'SANITÀ E ASSISTENZA SOCIALE', 'SANITÀ E ASSISTENZA SOCIALE', 'ATTIVITÀ ARTISTICHE, SPORTIVE, DI INTRATTENIMENTO E DIVERTIMENTO', 'ATTIVITÀ ARTISTICHE, SPORTIVE, DI INTRATTENIMENTO E DIVERTIMENTO', 'ATTIVITÀ ARTISTICHE, SPORTIVE, DI INTRATTENIMENTO E DIVERTIMENTO', 'ATTIVITÀ ARTISTICHE, SPORTIVE, DI INTRATTENIMENTO E DIVERTIMENTO', 'ALTRE ATTIVITÀ DI SERVIZI', 'ALTRE ATTIVITÀ DI SERVIZI', 'ALTRE ATTIVITÀ DI SERVIZI', 'ATTIVITÀ DI FAMIGLIE E CONVIVENZE COME DATORI DI LAVORO PER PERSONALE DOMESTICO', 'ATTIVITÀ DI FAMIGLIE E CONVIVENZE COME DATORI DI LAVORO PER PERSONALE DOMESTICO','ORGANIZZAZIONI ED ORGANISMI EXTRATERRITORIALI', 'NA')
  
  istat.sectors.map <- data.frame(ATECO.Sector.Code=ATECO.Sector.Code, ATECO.Sector.Name=ATECO.Sector.Name, ATECO.Sector.Description=ATECO.Sector.Description)
  
  # merging istat.sectors.map with aida using ATECO.Sector.Code key
  data <- merge(data,istat.sectors.map,by="ATECO.Sector.Code")
  data$ATECO.Sector.Code <- NULL
  data$ATECO.Sector.Name <- as.factor(data$ATECO.Sector.Name)
  rm(istat.sectors.map)
  
  return(data)
}




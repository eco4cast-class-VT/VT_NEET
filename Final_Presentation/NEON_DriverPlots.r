#NEON_DriverPlots.r
#Programmed by: Joshua M. Rady
#R. Q. Thomas Lab Virginia Tech
#Started: 4/27/2021
#
#Ecological Forecasting Initiative RCN
#Terrestrial Carbon & Water Fluxes NEON Forecast Challenge
#Team VT_NEET: Virginia Tech (FREC 7994)
#
#Description:---------------------------------------------------------------------------------------
# Plot the NEON driving data for temperature and incoming shortwave radiation.
#
#___________________________________________________________________________________________________

#Initialization:----------
rm(list=ls());#Clear memory.

library(neonUtilities)

#Globals:
NeonSiteIDs = c("BART", "KONZ", "OSBS", "SRER")

#Download the triple aspirated air temperature from NEON tower:
GetNEON_TAAT_Data <- function(dateStart, dateEnd)
{
  taatData = loadByProduct(dpID = "DP1.00003.001",
                           site = NeonSiteIDs,
                           startdate = dateStart,
                           enddate = dateEnd,
                           timeIndex = 30,
                           check.size = FALSE)
  return(taatData)
}

#Get the total downwelling SW radiation from the NEON tower pyranometer:
GetNEON_SW_Data <- function(dateStart, dateEnd)
{
  swData = loadByProduct(dpID = "DP1.00014.001",
                         site = NeonSiteIDs,
                         startdate = dateStart,
                         enddate = dateEnd,
                         timeIndex = 30,
                         check.size = FALSE)
  return(swData)
}

#Plot the driver data from the NEON towers for the period passed.
#The data will be downloaded if not supplied. This is slow so the GetNEON_XX_Data() functions should
#be used if the data will be used elsewhere.
PlotNEONdriverData <- function(dateStart = "2017-04", dateEnd = "2021-04",
                               taatData = NULL, swData = NULL)
{
  require(lubridate)
  
  #Common elements:
  qcFlagNames = c("Pass", "Fail")
  qcFlagColors = c("gray50", "red")
  
  #Get the triple aspirated air temperature if not supplied:
  if (is.null(taatData))
  {
    taatData = GetNEON_TAAT_Data(dateStart, dateEnd)
  }
  
  #Get the shortwave data if needed:
  if (is.null(swData))
  {
    swData = GetNEON_SW_Data(dateStart, dateEnd)
  }
  
  #Air temperature:
  tempDF = taatData$TAAT_30min
  
  #Center the temp in the 30 minute averaging window:
  #This is surprisingly tricky.  
  #This calculates the mean properly but the class is lost in the process. Setting SIMPLIFY = FALSE
  #retains the class but returns a list which is very large and converting it to a list loses the
  #class anyway.
  tempDF$Date = mapply(function (x,y) mean(c(x, y)), tempDF$startDateTime, tempDF$endDateTime)
  #Restore the class:
  tempDF$Date = as_datetime(tempDF$Date)
  
  tempDF$finalQF = as.factor(tempDF$finalQF)#QC flag, 0 = pass, 1 = fail
  
  #The QC flag is a problem here as it acts as a grouping variable causing the lines to be connected
  #by code value:
  # tempPlot = ggplot(tempDF, aes(x = Date, y = tempTripleMean, color = finalQF)) +
  #   geom_line() +
  #   labs(title = "NEON Site Air Temperature", y = "Temperature (C)") + 
  #   facet_wrap(~ siteID) +
  #   theme_bw()
  # print(tempPlot)
  
  #Improved version:
  tempPlot = ggplot(tempDF, aes(x = Date, y = tempTripleMean, color = finalQF)) +
    geom_point(size = 0.1) +
    scale_color_manual(values = qcFlagColors, labels = qcFlagNames) +
    labs(title = "NEON Site Air Temperature",
         y = "Temperature (C)",
         color = "QC Flag") + 
    facet_grid(rows = vars(siteID)) +
    theme_bw()
  print(tempPlot)
  
  #Shortwave:
  swDF = swData$SRDDP_30min
  
  #Center the temp in the 30 minute averaging window:
  swDF$Date = mapply(function (x,y) mean(c(x, y)), swDF$startDateTime, swDF$endDateTime)
  #Restore the class:
  swDF$Date = as_datetime(swDF$Date)
  
  swDF$gloRadFinalQF = as.factor(swDF$gloRadFinalQF)#QC flag, 0 = pass, 1 = fail
  
  #Plot by location with color for the QC code:
  swPlot = ggplot(swDF, aes(x = Date, y = gloRadMean, color = gloRadFinalQF)) +
    geom_point(size = 0.1) +
    scale_color_manual(values = qcFlagColors, labels = qcFlagNames) +
    labs(title = "NEON Site SW Radiation",
         y = expression(paste("Total Incoming Shortwave Radiation (W /", m^2, ")")),
         color = "QC Flag") + 
    facet_grid(rows = vars(siteID)) +
    theme_bw()
  print(swPlot)
}

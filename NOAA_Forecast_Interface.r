#NOAA_Forecast_Interface.r
#Programmed by: Joshua M. Rady
#R. Q. Thomas Lab Virginia Tech
#Started: 4/12/2021
#
#Ecological Forecasting Initiative RCN
#Terrestrial Carbon & Water Fluxes NEON Forecast Challenge
#Team VT_NEET: Virginia Tech (FREC 7994)
#
#Description:---------------------------------------------------------------------------------------
#  This file houses a set of functions for obtaining and loading NOAA weather forecasts.
#
#  The EFI NOAA Global Ensemble Forecast System (GEFS) V12 weather forecasts are stored at:
#https://data.ecoforecast.org/minio/drivers/
#  With an accessor function in:
#https://github.com/eco4cast/neon4cast/blob/main/R/read_forecast.R
#  See https://projects.ecoforecast.org/neon4cast-docs/meteorology-inputs.html for more information.
#
#Version History:-----------------------------------------------------------------------------------
#
#___________________________________________________________________________________________________

library(ncdf4)
library(lubridate)
#Install with:
# install.packages("remotes")
# remotes::install_github("eco4cast/neon4cast")
library(neon4cast)

#Globals:
NeonSiteIDs = c("BART", "KONZ", "OSBS", "SRER")
NOAA_Vars = c("air_temperature", "air_pressure", "relative_humidity",
              "surface_downwelling_longwave_flux_in_air",
              "surface_downwelling_shortwave_flux_in_air",
              "precipitation_flux", "specific_humidity", "cloud_area_fraction", "wind_speed")

NOAA_VarsShort = c("AirTemperature", "AirPressure", "RH", "Longwave", "Shortwave", "Precipitation",
                   "SpecificHumidity", "CloudFraction", "WindSpeed")

NOAA_VarUnits = c("Degrees K", "Pa", "", "W/m^2", "W/m^2", "kg/m^2/s", "", "", "m/s")

#Paths:
#We need to store the forecast data somewhere that will work across all installs of the project.
#We use a directory in the repository namd NOAA_Forecasts_Data that, at least for now, has it's
#contents ignored (via .gitignore).  This way forecasts are not synced as part of the repo but
#remain locally.
#See NOAA_ForecastPath() for the subdirectory structure.
ParentDir = getSrcDirectory(function(x) {x})#Dummy function.
ForecastDir = file.path(ParentDir, "NOAA_Forecasts_Data")

#Interface Functions:-------------------------------------------------------------------------------

#Download a NOAA forecast from the EFI bucket website:
#The neon4cast tools noaa.R download_noaa() function does this already so this just wraps that
#providing appropriate values for our setup.
#NeonSiteIDs: One or more NEON site ID codes. Default to all the Terrestrial challenge sites.
NOAA_DownloadForecast <- function(siteIDs = NeonSiteIDs, date = NULL)
{
  date = ValidateDate(date)#Check the date.
  
  #We only download the midnight forecasts (cycle = "00"), which is the default.  The default
  #behaviour it to download forcasts to a temporary directory but we use a persistent one:
  download_noaa(siteID = siteIDs, interval = "1hr", date = date, dir = ForecastDir)
  
  #download_noaa() returns nothing on failure so we check if the 
  for (siteID in siteIDs)
  {
    if (!dir.exists(NOAA_ForecastPath(siteID, date, checkDate = FALSE)))
    {
      stop(paste("Download for", siteID, "seems to have failed."))
    }
  }
}

#Return the NOAA forecast for the date requested as a data frame, downloading it if necessary:
NOAA_GetForecast <- function(siteIDs = NeonSiteIDs, date = NULL)
{
  date = ValidateDate(date)#Check the date.
  
  #Look to see if we already have the forecast downloaded:
  for (siteID in siteIDs)
  {
    #Assume that if the directory exists the download was complete.  Don't assume other sites were
    #downloaded:
    if (!dir.exists(NOAA_ForecastPath(siteID, date, checkDate = FALSE)))
    {
      #If a site is missing download it:
      print(paste("Downloading forecast for", siteID))
      NOAA_DownloadForecast(siteIDs = siteID, date = date)
    }
  }
  #We could collect the missing sites and download them all at once.
  
  #Read the netCDFs into a data frame with the locations and ensemble members as IDs:
  forecastDF = NULL
  for (siteID in siteIDs)
  {
    forecastPath = NOAA_ForecastPath(siteID, date, checkDate = FALSE)
    
    #We could just call list.files() and ingest all the files in the directory but this is safer
    #since it will catch missing files, can add ID variables as we go, and doesn't need to remove
    #ensemble member 0 after the fact.
    
    #Ignore ensemble member 0, which only has 16 days:
    for (j in 1:30)
    {
      #Example ensemble member name: NOAAGEFS_1hr_BART_2020-09-25T00_2020-10-30T00_ens30.nc
      #These names are pretty long.  Rather than computing the end date and assembling them we
      #use a wildcards and do some minor checking after the fact:
      memberFileEnd = paste("*_ens", formatC(j, width = 2, format = "d", flag = "0"), ".nc", sep = '')
      memberFilePath = Sys.glob(file.path(forecastPath, memberFileEnd))
      
      #There should only be one matching file:
      if (length(memberFilePath) > 1)
      {
        stop(paste("More than one match for forecast member", j))
      }
      if (!file.exists(memberFilePath))
      {
        stop(paste("The forecast member", j, "was not found at:", memberFilePath))
      }
      
      memberNC = nc_open(memberFilePath)
      
      #Get the ID variables:
      
      #Time is stored as the number of hours from the start time.  The start is stored in the units,
      #which we could parse, but we'll assume we have them right:
      hourFromStart = ncvar_get(memberNC, "time")
      #startTime = 
      #times = startTime + hours(hourFromStart)
      times = date + hours(hourFromStart)
      
      #The forecasts have been subsetted down to a single point so there will be a single set of
      #coordinates.  As a result these dimensions will also be dropped the variables upon extraction:
      latitude = ncvar_get(memberNC, "latitude")#Degrees N
      longitude = ncvar_get(memberNC, "longitude")#Degrees E (unsigned, 360 degree format)
      
      #Initialize the data frame with the ID variables:
      memberDF = data.frame(Date = times,#DateTime is more accurate but make a bad axis label.
                            Latitude = latitude, Longitude = longitude,
                            NEON_ID = siteID, Member = j)#EnsembleMember would be more explicit.
      
      #Add each variable as a column of the data frame:
      #We could get the variable names dynamically/
      
      #Some of the variable name are awkwardly long:
      # for (varID in NOAA_Vars)
      # {
      #   varData = ncvar_get(memberNC, varID)
      #   memberDF[[varID]] = as.vector(varData)
      # }
      
      #Use shorter names for variables:
      for (k in 1:length(NOAA_Vars))
      {
        varData = ncvar_get(memberNC, NOAA_Vars[k])
        memberDF[[NOAA_VarsShort[k]]] = as.vector(varData)
      }
      
      nc_close(memberNC)
      
      forecastDF = rbind(forecastDF, memberDF)
    }
  }
  
  #Convert some of the IDs:
  forecastDF$NEON_ID = factor(forecastDF$NEON_ID)
  forecastDF$Member = factor(forecastDF$Member)
  
  return(forecastDF)
}

#Utility Functions:---------------------------------------------------------------------------------

#Check that a date is valid and if not provided return the beginning of the most recent month:
#The forecasts takes a bit of time to process so it will be possible they won't be available for
#today.
#Note: Depending on the call sequence this may be called multiple times on the same date, which can
#make it a bit verbose.
ValidateDate <- function(date = NULL)
{
  functionName = match.call()[[1]]
  
  today = Sys.Date()#or lubridate::today(), which returns a POSIXct object.
  
  if (is.null(date))
  {
    return(as.Date(floor_date(today, "month")))
  }
  else
  {
    if (class(date) != "Date")
    {
      stop(paste(functionName, "expect a Date object."))
    }
    else if (date > today)
    {
      stop(paste("Date requested", date, "has not yet occured"))
    }
    else if (day(date) != 1)
    {
      warning("Date is not at the start of a month.")
    }
    return(date)
  }
}

#Return the appropriate path to find the specified NOAAforecast:
#The deeply nested directory structure below NOAA_Forecasts_Data is a result of the neon4cast code
#and the organization of the EFI data store.
#Forecasts are stored in nested directories labeled by site ID and then date.  Example:
#./NOAA_Forecasts_Data/noaa/noaa/NOAAGEFS_1hr/KONZ/NOAAGEFS_1hr/2012-01-03/00/
#
#The optional checkDate parameter can be used to prevent rechecking of the date.  The overhead of
#rechecking the date is small but it can lead to a lot of warnings.
NOAA_ForecastPath <- function(siteID, date = NULL, checkDate = TRUE)
{
  if (checkDate)
  {
    date = ValidateDate(date)
  }
  return(file.path(ForecastDir, "noaa", "noaa", "NOAAGEFS_1hr", siteID, date, "00"))
}

#An example to test the function and show it is working.
NOAA_ForecastTest <- function(dateString = "2021-01-15")
{
  functionName = match.call()[[1]]
  refCaption = paste(functionName, "() ", format(Sys.time(), "%a %D %X %Z"), sep = '')
  
  testDF = NOAA_GetForecast(date = as.Date(dateString))
  
  #plot = ggplot(testDF, aes(x = Date, y = air_temperature, color = Member)) +
  plot = ggplot(testDF, aes(x = Date, y = AirTemperature, color = Member)) +
    geom_line() +
    labs(title = "NOAA Forecast Interface Test Plot",
         subtitle = paste("Forcast period starting", dateString),
         caption = refCaption) +
    facet_wrap(~ NEON_ID)
  print(plot)
}

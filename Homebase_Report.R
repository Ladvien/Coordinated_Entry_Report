options(java.parameters = "-Xmx14336m")  ## memory set to 14 GB
# Argument guide
  # 1 = Name of Report
  # 2 = Folder path for HMIS data
  # 3 = VI-SPDAT File data
  # 4 = Staff Info File data
  # 5 = Executing directory path

#args <- commandArgs(trailingOnly = TRUE)
#nameOfReport <- args[1]
#hmisDataPath <- args[2]
#vispdatDataPath <- args[3]
#staffInfoDataPath <- args[4]
#executionPath <- args[5]

# PC
nameOfReport <- "Homebase_Report.R"
hmisDataPath <- "E:/Dropbox/HMIS/Warehouse/All Programs -- 5.1 -- 12-1-2016 to 2-28-2017"
vispdatDataPath <- "E:/Dropbox/HMIS/Coordinated_Entry_Report/VI-SPDAT and HUD Flat Export for SQL -- 3-6-2017.xlsx"
staffInfoDataPath <- "E:/Dropbox/HMIS/Coordinated_Entry_Report/Staff Contact Info for SQL -- 3-6-2017.xlsx"
executionPath <- "E:/Dropbox/HMIS/Coordinated_Entry_Report"
hmisFunctionsFilePath <- "E:/Dropbox/HMIS/HMIS_R_Functions/HMIS_R_Functions.R"
homebaseFunctionFilePath <- "E:/Dropbox/HMIS/Homebase_Function/Homebase_Function.R"
outputPath <- "E:/Dropbox/HMIS/Warehouse"
# Mac PC
#nameOfReport <- "Homebase_Report.R"
#hmisDataPath <- "C:/Users/Ladvien/Dropbox/HMIS/Warehouse/All Programs 12-18-2016 to 03-18-2017/"
#vispdatDataPath <- "C:/Users/Ladvien/Dropbox/HMIS/Coordinated_Entry_Report/VI-SPDAT and HUD Flat Export for SQL -- 3-6-2017.xlsx"
#staffInfoDataPath <- "C:/Users/Ladvien/Dropbox/HMIS/Coordinated_Entry_Report/Staff Contact Info for SQL -- 3-6-2017.xlsx"
#executionPath <- "C:/Users/Ladvien/Dropbox/HMIS/Coordinated_Entry_Report"
#hmisFunctionsFilePath <- "C:/Users/Ladvien/Dropbox/HMIS/HMIS_R_Functions/HMIS_R_Functions.R"
#homebaseFunctionFilePath <- "C:/Users/Ladvien/Dropbox/HMIS/Homebase_Function/Homebase_Function.R"

# Mac
#nameOfReport <- "Homebase_Report.R"
#hmisDataPath <- "/Users/user/Dropbox/HMIS/Coordinated_Entry_Report/All Programs 12-18-2016 to 03-18-2017"
#vispdatDataPath <- "/Users/user/Dropbox/HMIS/Coordinated_Entry_Report/VI-SPDAT and HUD Flat Export for SQL -- 3-6-2017.xlsx"
#staffInfoDataPath <- "/Users/user/Dropbox/HMIS/Coordinated_Entry_Report/Staff Contact Info for SQL -- 3-6-2017.xlsx"
#executionPath <- "/Users/user/Dropbox/HMIS/Coordinated_Entry_Report"
#hmisFunctions <- "/Users/user/Dropbox/HMIS/HMIS_R_Functions/HMIS_R_Functions.R"
cat("arg1: ")
cat(nameOfReport)
cat("\n") 
cat("arg2: ")
cat(hmisDataPath)
cat("\n") 
cat("arg3: ")
cat(vispdatDataPath)
cat("\n")
cat("arg4: ")
cat(staffInfoDataPath)
cat("\n")
cat("arg5: ")
cat(executionPath)
cat("\n")

source(homebaseFunctionFilePath)

homebase_all <- homebase(hmisDataPath,
                 vispdatDataPath,
                 staffInfoDataPath,
                 executionPath,
                 hmisFunctionsFilePath)

homebase_housed <- sqldf("SELECT *
                         FROM homebase_all
                         WHERE ActiveInPH IS 'Yes'
                         ORDER BY ChronicallyHomeless DESC, ScoreVISPDAT DESC
                         ")

# Filter client to PersonalIDs with Entry or Update in last 90 days
homebase_active <- sqldf("SELECT *
                  FROM homebase_all
                  WHERE (
                          RecentHUDEntryDate > DATE('NOW', '-90 DAY')
                          OR (MostRecentHUDAssess > DATE('NOW', '-90 DAY'))
                        ) 
                        AND
                        (
                          (DateOfVISPDAT > DATE('NOW', '-90 DAY'))
                        )
                  AND
                  ActiveInPH IS NOT 'Yes'
                  ORDER BY ChronicallyHomeless DESC, ScoreVISPDAT DESC
                  ")

tmp_possibly_active <- sqldf("SELECT *
                             FROM homebase_all
                             WHERE ( MostRecentHUDAssess > DATE('NOW', '-90 DAY')) 
                             OR
                             (
                              (DateOfVISPDAT > DATE('NOW', '-90 DAY'))
                             )
                             AND
                             ActiveInPH IS NOT 'Yes'
                             ORDER BY ChronicallyHomeless DESC, ScoreVISPDAT DESC
                             ")

homebase_possibly_active <- sqldf("SELECT a.*
                                  FROM tmp_possibly_active a
                                  LEFT JOIN homebase_active b
                                  ON a.PersonalID=b.PersonalID
                                  WHERE b.PersonalID IS NULL
                                  ")

homebase_psh <- sqldf("SELECT *
                      FROM homebase_active
                      WHERE ChronicallyHomeless = 'Yes'
                      ")

homebase_rrh <- sqldf("SELECT *
                      FROM homebase_active
                      WHERE ChronicallyHomeless IS NOT 'Yes'
                      ")


homebase_assistToList <- sqldf("SELECT DISTINCT StaffName, COUNT(StaffName) As 'AssistToList'
                    FROM homebase_active
                    WHERE ActiveInPH IS NOT 'Yes'
                    GROUP BY StaffName
                    ORDER BY AssistToList DESC
                    ")


detach("package:XLConnect", unload = TRUE)
library(xlsx)
setwd(outputPath)
write.xlsx(homebase_psh, file="Homebase_v2.xlsx", sheetName="EligibleForPSH", row.names=FALSE, showNA=FALSE)
write.xlsx(homebase_rrh, file="Homebase_v2.xlsx", sheetName="EligibleForRRH", row.names=FALSE, showNA=FALSE, append=TRUE)
write.xlsx(homebase_possibly_active, file="Homebase_v2.xlsx", sheetName="Possibly Active", row.names=FALSE, showNA=FALSE, append=TRUE)
write.xlsx(homebase_housed, file="Homebase_v2.xlsx", sheetName="Housed", row.names=FALSE, showNA=FALSE, append=TRUE)
#write.xlsx(homebase_all, file="Homebase.xlsx", sheetName="All", row.names=FALSE, showNA=FALSE, append=TRUE)
write.xlsx(homebase_assistToList, file="Homebase_v2.xlsx", sheetName="Assist-to-List", row.names=FALSE, showNA=FALSE, append=TRUE)


homebase <- function(
  hmisDataPath,
  vispdatDataPath,
  staffInfoDataPath,
  executionPath,
  hmisFunctionsFilePath,
  viSpdat2DataPath) {

  library("tcltk")
  
  # Load the weights for progress bar
  loadingPackagesIncrement <- 2
  loadingHMISDataIncrement <- 10
  addDisabilitiesIncrement <- 5
  householdIdIncrement <- 4
  calculatingAgeIncrement <- 1
  gettingEnrollmentsIncrement <- 10
  gettingStaffInfoIncrement <- 5
  calculatingCHIncrement <- 10
  addVispdatIncrement <- 5
  getFamilyWithChildIncrement <- 5
  loadServicesIncrement <- 15
  nbnStaysIncrement <- 5
  outreachContactsIncrement <- 5
  outreachAndNbnCountIncrement <- 5
  makeHmisCodesReadableIncrement <- 2
  formatHomebaseIncrement <- 1
  
  # Find the progress bar max.
  total <- (loadingPackagesIncrement +
              loadingHMISDataIncrement +
              addDisabilitiesIncrement +
              householdIdIncrement +
              calculatingAgeIncrement +
              gettingEnrollmentsIncrement +
              gettingStaffInfoIncrement +
              calculatingCHIncrement +
              addVispdatIncrement +
              getFamilyWithChildIncrement +
              loadServicesIncrement +
              nbnStaysIncrement +
              outreachAndNbnCountIncrement +
              makeHmisCodesReadableIncrement +
              formatHomebaseIncrement
  )
      # Initialize progress bar
  pbCounter = 0
  pb <- tkProgressBar(title = "Homebase Function", min = 0,
                      max = total, width = 300)
  
  ###### START ##########
  setTkProgressBar(pb, pbCounter, label = "Loading Packages")

  # There needs to be a lot of allocated memory for Java for
  # XLConnect to work.
    options(java.parameters = "-Xmx14336m") ## memory set to 14 GB

  library(sqldf)
  library(XLConnect)
  library(xlsx)


  # Load the HMIS functions.
  source(hmisFunctionsFilePath)
  
  # Return to execution path.
  setwd(executionPath)
  
  # Update progress bar
  pbCounter <- pbCounter + loadingPackagesIncrement
  setTkProgressBar(pb, pbCounter, label = "Loading HMIS Data")
  
  # Load HMIS Data
  client <- loadClient(hmisDataPath)
  enrollment <- loadEnrollment(hmisDataPath)
  disabilities <- loadDisabilities(hmisDataPath)
  exit <- loadExit(hmisDataPath)
  project <- loadProject(hmisDataPath)
  enrollmentCoc <- loadEnrollmentCoc(hmisDataPath)
  
  # Return to execution path.
  setwd(executionPath)
  
  pbCounter <- pbCounter + loadingHMISDataIncrement
  setTkProgressBar(pb, pbCounter, label = "Getting Disabilities")
  
  # Takes the Disabilities.csv and breaks out the disabilities reported
  # for each participants into individual elements with binary resposnes.
  client <- addDisabilityInfoToClient(client, disabilities)
  
  # Update progress bar
  pbCounter <- pbCounter + addDisabilitiesIncrement
  setTkProgressBar(pb, pbCounter, label = "Getting Household IDs")
  
  # Gets the PersonalIDs of participants with a HUD Entry or Update in last 90 days.
  enrollmentCoc <- getMostRecentRecordsPerId(enrollmentCoc, "PersonalID", "DateCreated")
  enrollmentCoc$DateCreated <- as.character(enrollmentCoc$DateCreated)
  
  # Get Household IDs from EnrollmentCoc.csv
  client <- sqldf("SELECT DISTINCT a.*, b.HouseholdID
                  FROM client a
                  LEFT JOIN enrollmentCoc b
                  ON a.PersonalID=b.PersonalID
                  ")
  
  # Update progress bar
  pbCounter <- pbCounter + householdIdIncrement
  setTkProgressBar(pb, pbCounter, label = "Calculating Age")
  
  # Calculate age
  client <- sqldf("SELECT DISTINCT *, (DATE('NOW') - DATE(DOB)) As 'Age' FROM client")
  
  pbCounter <- pbCounter + calculatingAgeIncrement
  setTkProgressBar(pb, pbCounter, label = "Getting Enrollments")
  
  # Filters to most recent HUD Assessment per participant
  df1 <- getMostRecentRecordsPerId(enrollment, "PersonalID", "EntryDate")
  
  # Adds a 'MaxEntryDate' flag to enrollment
  enrollment <- sqldf("SELECT a.*, b.MaxEntryDate
                      FROM enrollment a
                      LEFT JOIN df1 b
                      ON a.ProjectEntryID=b.ProjectEntryID
                      ")
  
  # Get Project Info for Enrollment
  enrollmentCoc <- addProjectInfoToEnrollment(enrollmentCoc, project)
  
  df3 <- sqldf("SELECT PersonalID, InformationDate As 'MostRecentHUDAssess', UserID As 'StaffID', ProjectName, ProjectType
               FROM enrollmentCoc")
  df3 <- makeProjectTypeReadable(df3)
  pbCounter <- pbCounter + gettingStaffInfoIncrement
  setTkProgressBar(pb, pbCounter, label = "Calculating Chronically Homeless")
  
  # Adds a ChronicallyHomeless flag
  df4 <- addChronicallyHomelessFlagToClient(client, df1)
  # Adds a ActiveInPh flag
  df5 <- getClientsInPH(enrollment, exit, project)
  
  # Returns flags to clientDf
  targetClient <- sqldf("SELECT DISTINCT a.*, b.'ActiveInPh'
                        FROM df4 a
                        LEFT JOIN df5 b
                        ON a.PersonalID=b.PersonalID
                        ")
  
  colnames(targetClient)[32] <- "RecentHUDEntryDate"
  
  # Load staff information
  staffInfo <- readWorksheetFromFile(staffInfoDataPath, sheet = 1, startRow = 1)
  colnames(staffInfo)[1] <- "StaffID"
  staffInfo <- sqldf("SELECT DISTINCT StaffID, Name, Email FROM staffInfo")
  
  # Find the staff information for who completed each HUD Assessment.
  df7 <- sqldf("SELECT a.*, b.Name As 'StaffName', b.Email As 'StaffEmail'
               FROM df3 a
               LEFT JOIN staffInfo b
               ON a.StaffID=b.StaffID
               ")
  remove(list = c("staffInfo"))
  
  # Add the Staff and Project information back to client list.
  targetClient <- sqldf("SELECT a.*, b.MostRecentHUDAssess, b.StaffName, b.StaffEmail, b.ProjectName As 'LastProgramInContact', b.ReadableProjectType As 'LastProjectTypeContacted'
                        FROM targetClient a
                        LEFT JOIN df7 b
                        ON a.PersonalID=b.PersonalID
                        ")
  
  targetClient <- subset(targetClient)
  
  # Cleanup
  remove(list = c("df4", "df3", "df5", "df1", "enrollmentCoc"))
  
  pbCounter <- pbCounter + calculatingCHIncrement
  setTkProgressBar(pb, pbCounter, label = "Adding VI-SPDAT")
  
  # Load VI-SPDAT information
  viSpdat <- readWorksheetFromFile(vispdatDataPath, sheet = 1, startRow = 1)
  viSpdat2 <- readWorksheetFromFile(viSpdat2DataPath, sheet = 1, startRow = 1)

  # Clean up VI-SPDAT1 formatting
  colnames(viSpdat)[6] <- "DateOfVISPDAT"
  viSpdat$DateOfVISPDAT <- as.character(viSpdat$DateOfVISPDAT)
  viSpdat$Participant.Enterprise.Identifier <- gsub("-", "", viSpdat$Participant.Enterprise.Identifier)
  viSpdat$Family.Enterprise.Identifier <- gsub("-", "", viSpdat$Family.Enterprise.Identifier)
  viSpdat$Family.Enterprise.Identifier <- gsub("\\{", "", viSpdat$Family.Enterprise.Identifier)
    viSpdat$Family.Enterprise.Identifier <- gsub("\\}", "", viSpdat$Family.Enterprise.Identifier)
    colnames(viSpdat)[1] <- "PersonalID"

  # Clean up VI-SPDAT2 formatting
  viSpdat2$DateOfVISPDAT <- as.character(viSpdat2$DateOfVISPDAT)
  viSpdat2$PersonalID <- gsub("-", "", viSpdat2$PersonalID)
    
    # Get most recent VI-SPDAT per client.
  viSpdat <- getMostRecentRecordsPerId(viSpdat, "PersonalID", "DateOfVISPDAT")
  viSpdat2 <- getMostRecentRecordsPerId(viSpdat2, "PersonalID", "DateOfVISPDAT")

    # Shape VI-SPDAT to look like VI-SPDAT 2
    viSpdat <- sqldf("SELECT PersonalID, DateOfVISPDAT As 'DateOfVISPDAT', '' As VISPDATTotalFamilyScore, '' As VISPDATTotalYouthScore, ScoreVISPDAT As 'VISPDATTotalIndividualScore', 'Individual' As TypeOfViSpdat FROM viSpdat")
    viSpdat2 <- sqldf("SELECT PersonalID, DateOfVISPDAT, VISPDATTotalFamilyScore, VISPDATTotalYouthScore, VISPDATTotalIndividualScore, TypeOfViSpdat FROM viSpdat2")

    allVispdat <- rbind(viSpdat, viSpdat2)

    # Get max between new and old VI-SPDAT
    allVispdat <- getMostRecentRecordsPerId(allVispdat, "PersonalID", "DateOfVISPDAT")

    allVispdat <- unique(allVispdat)

   
    allVispdat$VISPDATTotalIndividualScore[allVispdat$VISPDATTotalIndividualScore == ''] <- '0'
    allVispdat$VISPDATTotalFamilyScore[allVispdat$VISPDATTotalFamilyScore == ''] <- '0'
    allVispdat$VISPDATTotalYouthScore[allVispdat$VISPDATTotalYouthScore == ''] <- '0'

    allVispdat$VISPDATTotalIndividualScore[is.na(allVispdat$VISPDATTotalIndividualScore)] <- '0'
    allVispdat$VISPDATTotalFamilyScore[is.na(allVispdat$VISPDATTotalFamilyScore)] <- '0'
    allVispdat$VISPDATTotalYouthScore[is.na(allVispdat$VISPDATTotalYouthScore)] <- '0'


    allVispdat$VISPDATTotalIndividualScore <- as.integer(allVispdat$VISPDATTotalIndividualScore)
    allVispdat$VISPDATTotalFamilyScore <- as.integer(allVispdat$VISPDATTotalFamilyScore)
    allVispdat$VISPDATTotalYouthScore <- as.integer(allVispdat$VISPDATTotalYouthScore)
    
    allVispdat <- subset(allVispdat)
    # Make one column for score.
    #allVispdat <- sqldf("SELECT *, CASE 
                                    #WHEN (VISPDATTotalFamilyScore IS '' 
                                          #AND VISPDATTotalYouthScore IS '') 
                                          #THEN VISPDATTotalIndividualScore
                                    #WHEN (VISPDATTotalFamilyScore IS '' 
                                          #AND VISPDATTotalIndividualScore IS '') 
                                          #THEN VISPDATTotalYouthScore
                                          #ELSE VISPDATTotalFamilyScore
                                   #END As 'ScoreVISPDAT'
                        #FROM allVispdat
                        #ORDER BY ScoreVISPDAT DESC
                      #")

    allVispdat <- sqldf("SELECT *, CASE 
                                    WHEN (VISPDATTotalFamilyScore <= VISPDATTotalIndividualScore
                                          AND VISPDATTotalYouthScore <= VISPDATTotalIndividualScore) 
                                          THEN VISPDATTotalIndividualScore
                                    WHEN (VISPDATTotalFamilyScore <= VISPDATTotalYouthScore
                                          AND VISPDATTotalIndividualScore <= VISPDATTotalYouthScore) 
                                          THEN VISPDATTotalYouthScore
                                          ELSE VISPDATTotalFamilyScore
                                   END As 'ScoreVISPDAT'
                        FROM allVispdat
                        ORDER BY ScoreVISPDAT DESC
                      ")

    allVispdat$ScoreVISPDAT <- as.numeric(allVispdat$ScoreVISPDAT)

  targetClient <- subset(targetClient)


  # Add VI-SPDAT scores and dates to the client list.
  targetClient <- sqldf("SELECT a.*, b.ScoreVISPDAT, b.DateOfVISPDAT, b.TypeOfVispdat
                        FROM targetClient a
                        LEFT JOIN allVispdat b
                        ON a.PersonalID=b.PersonalID
                        ")
  
  # Just in case, make date SQLite friendly.
  targetClient$DateOfVISPDAT <- as.character(targetClient$DateOfVISPDAT)
  
  remove(list = c("viSpdat", "viSpdat2", "allVispdat"))
  
  pbCounter <- pbCounter + addVispdatIncrement
  setTkProgressBar(pb, pbCounter, label = "Getting Family with Child")
  
  # Get all the households which contain an adult.
  olderThan18 <- sqldf("SELECT HouseholdID
                       FROM client
                       WHERE Age > 17
                       ")
  
  # Get all the households which contain an child.
  youngerThan18 <- sqldf("SELECT HouseholdID
                         FROM client
                         WHERE Age < 18
                         ")
  
  
  # Create a flag for families with children
  familyWithChildren <- sqldf("SELECT a.HouseholdID, 'Yes' As FamilyWithChildren
                              FROM youngerThan18 a
                              INNER JOIN olderThan18 b
                              ON a.HouseholdID=b.HouseholdID
                              ")
  
  
  # Add the family flag back to the client list.
  targetClient <- sqldf("SELECT DISTINCT a.*, b.FamilyWithChildren
                        FROM targetClient a
                        LEFT JOIN familyWithChildren b
                        ON a.HouseholdID=b.HouseholdID
                        ")
  
  remove(list = c("olderThan18", "youngerThan18", "familyWithChildren"))
  
  targetClient <- subset(targetClient)
  pbCounter <- pbCounter + getFamilyWithChildIncrement
  setTkProgressBar(pb, pbCounter, label = "Loading Services")
  
  
  # Free up some memory before attempting Services.csv
  remove(list = c("client", "enrollment", "disabilities", "exit", "project"))
  
  setwd(hmisDataPath)
  
  # Load services -- large files.
  services <- loadServices()
  
  pbCounter <- pbCounter + loadServicesIncrement
  setTkProgressBar(pb, pbCounter, label = "Getting Night-by-Night Stays")
  
  # Get all of te NBN service entries.
  clientNbn <- sqldf("SELECT *
                     FROM services
                     WHERE RecordType = 200
                     ")
  # Count the number of NBN check-ins per client.  Make sure the date as distinct, due to ETO duplicate bed
  # stays when data is pulled using a program-group including two-shelters.
  daysCheckedInNBN <- sqldf("SELECT DISTINCT(PersonalID), COUNT (DISTINCT(DateProvided)) As 'NonProgramShelterNights'
                            FROM clientNbn
                            GROUP BY PersonalID
                            ")
  
  pbCounter <- pbCounter + nbnStaysIncrement
  setTkProgressBar(pb, pbCounter, label = "Getting Outreach Contacts")
  
  # Get all Outreach Contact services.
  clientOutreach <- sqldf("SELECT *
                          FROM services
                          WHERE RecordType = 12
                          ")
  
  # Count all Outreach Contacts per client.  Count distinct is to get around ETO bug when data is
  # pulled using a program group which includes two outreach agencies.
  outreachContacts <- sqldf("SELECT DISTINCT(PersonalID), COUNT (DISTINCT(DateProvided)) As 'NumberOutreachContacts'
                            FROM clientOutreach
                            GROUP BY PersonalID
                            ")
  
  pbCounter <- pbCounter + outreachContactsIncrement
  setTkProgressBar(pb, pbCounter, label = "Adding Outreach and NBN Count")
  
  # Add NBN stays to client list.
  targetClient <- sqldf("SELECT a.*, b.NonProgramShelterNights
                        FROM targetClient a
                        LEFT JOIN daysCheckedInNBN b
                        ON a.PersonalID=b.PersonalID
                        ")
  
  # Add Outreach Contacts to client list.
  targetClient <- sqldf("SELECT a.*, b.NumberOutreachContacts
                        FROM targetClient a
                        LEFT JOIN outreachContacts b
                        ON a.PersonalID=b.PersonalID
                        ")
  
  remove(list = c("services", "clientOutreach", "daysCheckedInNBN", "clientNbn", "outreachContacts"))
  
  pbCounter <- pbCounter + outreachAndNbnCountIncrement
  setTkProgressBar(pb, pbCounter, label = "Make HMIS Codes Readable")
  
  setwd(executionPath)
  
  # Make HMIS codes human readable.
  targetClient <- combineRaceColumnsAndMakeReadable(targetClient)
  targetClient <- makeGenderReadable(targetClient)
  targetClient <- makeEthnicityReadable(targetClient)
  targetClient <- makeVeteranStatusReadable(targetClient)
  
  
  pbCounter <- pbCounter + makeHmisCodesReadableIncrement
  setTkProgressBar(pb, pbCounter, label = "Format Homebase Dataframe")
  
  # Pull and format the client list.
  homebase_all <- sqldf("SELECT DISTINCT
                        PersonalID,
                        FirstName,
                        MiddleName,
                        LastName,
                        NameSuffix,
                        SSN,
                        DOB,
                        Age,
                        Gender,
                        Race,
                        Ethnicity,
                        VeteranStatus,
                        HouseholdID,
                        ChronicallyHomeless,
                        NumberOutreachContacts,
                        NonProgramShelterNights,
                        RecentHUDEntryDate,
                        MostRecentHUDAssess,
                        FamilyWithChildren,
                        ScoreVISPDAT,
                        TypeOfViSpdat,
                        DateOfVISPDAT,
                        PhysicalDisability,
                        DevelopmentalDisability,
                        ChronicHealthCondition,
                        a.'HIV/AIDS',
                        MentalHealthProblem,
                        SubstanceAbuse,
                        StaffName,
                        StaffEmail,
                        LastProgramInContact,
                        LastProjectTypeContacted,
                        ActiveInPH
                        FROM targetClient a
                        ")

    pbCounter <- pbCounter + formatHomebaseIncrement
    setTkProgressBar(pb, pbCounter, label = "Homebase Complete")

    # Clean up
    close(pb)
    rm(list = setdiff(ls(), "homebase_all"))
    # Retun list.
    homebase_all
}
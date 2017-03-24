homebase <- function(
        hmisDataPath,
        vispdatDataPath,
        staffInfoDataPath,
        executionPath,
        hmisFunctionsFilePath) {
    library("tcltk")

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
    pbCounter = 0

    pb <- tkProgressBar(title = "Homebase Function", min = 0,
                    max = total, width = 300)

    setTkProgressBar(pb, pbCounter, label = "Loading Packages")
    options(java.parameters = "-Xmx14336m") ## memory set to 14 GB
    library("sqldf")
    library("XLConnect")
    library(xlsx)
    source(hmisFunctionsFilePath)
    pbCounter <- pbCounter + loadingPackagesIncrement
    setTkProgressBar(pb, pbCounter, label = "Loading Packages")


    setwd(executionPath)

    setTkProgressBar(pb, pbCounter, label = "Loading HMIS Data")
    # Load HMIS Data
    client <- loadClient(hmisDataPath)
    enrollment <- loadEnrollment(hmisDataPath)
    disabilities <- loadDisabilities(hmisDataPath)
    exit <- loadExit(hmisDataPath)
    project <- loadProject(hmisDataPath)
    enrollmentCoc <- loadEnrollmentCoc(hmisDataPath)

    pbCounter <- pbCounter + loadingHMISDataIncrement
    setTkProgressBar(pb, pbCounter, label = "Getting Disabilities")
    client <- addDisabilityInfoToClient(client, disabilities)

    pbCounter <- pbCounter + addDisabilitiesIncrement
    setTkProgressBar(pb, pbCounter, label = "Getting Household IDs")
    # Get Household IDs
    client <- sqldf("SELECT DISTINCT a.*, b.HouseholdID
                FROM client a
                LEFT JOIN enrollmentCoc b
                ON a.PersonalID=b.PersonalID
                ")

    pbCounter <- pbCounter + householdIdIncrement
    setTkProgressBar(pb, pbCounter, label = "Calculating Age")

    # Calculate age
    client <- sqldf("SELECT DISTINCT *, (DATE('NOW') - DATE(DOB)) As 'Age' FROM client")


    pbCounter <- pbCounter + calculatingAgeIncrement
    setTkProgressBar(pb, pbCounter, label = "Getting Enrollments")

    # Filters to most recent HUD Assessment per participant
    mostRecentEnrollment <- getMostRecentRecordsPerId(enrollment, "PersonalID", "EntryDate")

    # Adds a 'MaxEntryDate' flag to enrollment
    enrollment <- sqldf("SELECT a.*, b.MaxEntryDate
              FROM enrollment a
              LEFT JOIN mostRecentEnrollment b
              ON a.ProjectEntryID=b.ProjectEntryID
              ")

    # Get Project Info for Enrollment
    enrollmentCoc <- addProjectInfoToEnrollment(enrollmentCoc, project)

    #maxDisabilityInfo <- getMostRecentRecordsPerId(disabilities, "PersonalID", "InformationDate")
    disabilities <- makeDisabilityTypeReadable(disabilities)

    # Gets the PersonalIDs of participants with a HUD Entry or Update in last 90 days.
    maxEnrollmentsOrUpdate <- getMostRecentRecordsPerId(enrollmentCoc, "PersonalID", "DateCreated")
    maxEnrollmentsOrUpdate$DateCreated <- as.character(maxEnrollmentsOrUpdate$DateCreated)
    enrollmentsOrUpdateInRange <- activeFilter(maxEnrollmentsOrUpdate, 'DateCreated', 'DateCreated', as.character(Sys.Date() - 90), as.character(Sys.Date()))

    remove(list = c("maxEnrollmentsOrUpdate"))

    pbCounter <- pbCounter + gettingEnrollmentsIncrement
    setTkProgressBar(pb, pbCounter, label = "Getting Staff IDs")
    staffInfo <- readWorksheetFromFile(staffInfoDataPath, sheet = 1, startRow = 1)
    setwd(executionPath)

    colnames(staffInfo)[1] <- "StaffID"
    staffInfo <- sqldf("SELECT DISTINCT StaffID, Name, Email FROM staffInfo")

    enrollmentsOrUpdateInRangeWithStaffInfo <- sqldf("SELECT a.*, b.Name As 'StaffName', b.Email As 'StaffEmail'
                                                 FROM enrollmentsOrUpdateInRange a
                                                 LEFT JOIN staffInfo b
                                                 ON a.UserID=b.StaffID
                                                 ")
    remove(list = c("staffInfo"))

    personalIdsWithEntryOrUpdateInRange <- sqldf("SELECT PersonalID, StaffName, StaffEmail, ProjectName, ReadableProjectType As 'ProjectType', InformationDate As 'MostRecentHUDAssess' 
                                             FROM enrollmentsOrUpdateInRangeWithStaffInfo")

    pbCounter <- pbCounter + gettingStaffInfoIncrement
    setTkProgressBar(pb, pbCounter, label = "Calculating Chronically Homeless")

    # Adds a ChronicallyHomeless flag
    clientsCh <- addChronicallyHomelessFlagToClient(client, mostRecentEnrollment)
    # Adds a ActiveInPh flag
    clientsInPh <- getClientsInPH(enrollment, exit, project)

    # Returns flags to clientDf
    targetClient <- sqldf("SELECT DISTINCT a.*, b.'ActiveInPh'
              FROM clientsCh a
              LEFT JOIN clientsInPh b
              ON a.PersonalID=b.PersonalID
              ")

    colnames(targetClient)[32] <- "RecentHUDEntryDate"

    # Filter client to PersonalIDs with Entry or Update in last 90 days
    # targetClient <- sqldf("SELECT a.*, b.StaffName, b.StaffEmail, b.ProjectName As 'LastProgramInContact', b.ProjectType As 'LastProjectContacted', MostRecentHUDAssess
    #                 FROM targetClient a
    #                 INNER JOIN personalIdsWithEntryOrUpdateInRange b
    #                 ON a.PersonalID=b.PersonalID
    #                 ")

    targetClient <- sqldf("SELECT a.*, b.StaffName, b.StaffEmail, b.ProjectName As 'LastProgramInContact', b.ProjectType As 'LastProjectContacted', MostRecentHUDAssess
                      FROM targetClient a
                      LEFT JOIN personalIdsWithEntryOrUpdateInRange b
                      ON a.PersonalID=b.PersonalID
                      ")

    # Cleanup
    remove(list = c("clientsCh", "personalIdsWithEntryOrUpdateInRange", "clientsInPh", "mostRecentEnrollment", "enrollmentsOrUpdateInRange", "enrollmentCoc", "enrollmentsOrUpdateInRangeWithStaffInfo"))

    pbCounter <- pbCounter + calculatingCHIncrement
    setTkProgressBar(pb, pbCounter, label = "Adding VI-SPDAT")

    viSpdat <- readWorksheetFromFile(vispdatDataPath, sheet = 1, startRow = 1)
    colnames(viSpdat)[6] <- "DateOfVISPDAT"
    viSpdat$DateOfVISPDAT <- as.character(viSpdat$DateOfVISPDAT)
    viSpdat$Participant.Enterprise.Identifier <- gsub("-", "", viSpdat$Participant.Enterprise.Identifier)
    viSpdat$Family.Enterprise.Identifier <- gsub("-", "", viSpdat$Family.Enterprise.Identifier)
    viSpdat$Family.Enterprise.Identifier <- gsub("\\{", "", viSpdat$Family.Enterprise.Identifier)
    viSpdat$Family.Enterprise.Identifier <- gsub("\\}", "", viSpdat$Family.Enterprise.Identifier)

    colnames(viSpdat)[1] <- "PersonalID"

    viSpdat <- getMostRecentRecordsPerId(viSpdat, "PersonalID", "DateOfVISPDAT")

    targetClient <- subset(targetClient)
    targetClient <- sqldf("SELECT a.*, b.ScoreVISPDAT, b.DateOfVISPDAT
                      FROM targetClient a
                      LEFT JOIN viSpdat b
                      ON a.PersonalID=b.PersonalID
                      ")

    targetClient$DateOfVISPDAT <- as.character(targetClient$DateOfVISPDAT)

    # targetClient <- sqldf("SELECT DISTINCT *
    #                       FROM targetClient
    #                       WHERE MostRecentHUDAssess > DATE('NOW', '-90 DAY')
    #                       AND DateOfVISPDAT > DATE('NOW', '-90 DAY')
    #                       ")

    remove(list = c("viSpdat"))

    pbCounter <- pbCounter + addVispdatIncrement
    setTkProgressBar(pb, pbCounter, label = "Getting Family with Child")

    olderThan18 <- sqldf("SELECT HouseholdID
              FROM client
              WHERE Age > 17
              ")

    youngerThan18 <- sqldf("SELECT HouseholdID
                       FROM client
                       WHERE Age < 18
                       ")

    # Create FamilyWithChildren flag
    familyWithChildren <- sqldf("SELECT a.HouseholdID, 'Yes' As FamilyWithChildren
                            FROM youngerThan18 a
                            INNER JOIN olderThan18 b
                            ON a.HouseholdID=b.HouseholdID
                            ")



    targetClient <- sqldf("SELECT DISTINCT a.*, b.FamilyWithChildren
                      FROM targetClient a
                      LEFT JOIN familyWithChildren b
                      ON a.HouseholdID=b.HouseholdID
                      ")

    remove(list = c("olderThan18", "youngerThan18", "familyWithChildren"))



    targetClient <- subset(targetClient)
    pbCounter <- pbCounter + getFamilyWithChildIncrement
    setTkProgressBar(pb, pbCounter, label = "Loading Services")


    # enrollmentAndProjectInfo <- sqldf("SELECT a.*, b.ProjectID, b.ProjectName 
    #                                   FROM enrollment a
    #                                   INNER JOIN project b
    #                                   ON a.ProjectID=b.ProjectID
    #                                   ")
    # 
    # enrollmentAndProjectInfo <- subset(enrollmentAndProjectInfo)
    # Free up some memory before attempting Services.csv
    remove(list = c("client", "enrollment", "disabilities", "exit", "project"))

    setwd(hmisDataPath)
    services <- loadServices()

    pbCounter <- pbCounter + loadServicesIncrement
    setTkProgressBar(pb, pbCounter, label = "Getting Night-by-Night Stays")

    clientNbn <- sqldf("SELECT *
                   FROM services
                   WHERE RecordType = 200
                   ")
    daysCheckedInNBN <- sqldf("SELECT DISTINCT(PersonalID), COUNT (DISTINCT(DateProvided)) As 'NonProgramShelterNights'
                          FROM clientNbn
                          GROUP BY PersonalID
                          ")

    pbCounter <- pbCounter + nbnStaysIncrement
    setTkProgressBar(pb, pbCounter, label = "Getting Outreach Contacts")

    clientOutreach <- sqldf("SELECT *
                        FROM services
                        WHERE RecordType = 12
                        ")

    outreachContacts <- sqldf("SELECT DISTINCT(PersonalID), COUNT (DISTINCT(DateProvided)) As 'NumberOutreachContacts'
                          FROM clientOutreach
                          GROUP BY PersonalID
                          ")

    pbCounter <- pbCounter + outreachContactsIncrement
    setTkProgressBar(pb, pbCounter, label = "Adding Outreach and NBN Count")

    targetClient <- sqldf("SELECT a.*, b.NonProgramShelterNights
                      FROM targetClient a
                      LEFT JOIN daysCheckedInNBN b
                      ON a.PersonalID=b.PersonalID
                      ")

    targetClient <- sqldf("SELECT a.*, b.NumberOutreachContacts
                      FROM targetClient a
                      LEFT JOIN outreachContacts b
                      ON a.PersonalID=b.PersonalID
                      ")

    remove(list = c("services", "clientOutreach", "daysCheckedInNBN", "clientNbn", "outreachContacts"))

    pbCounter <- pbCounter + outreachAndNbnCountIncrement
    setTkProgressBar(pb, pbCounter, label = "Make HMIS Codes Readable")

    setwd(executionPath)
    # Decode columns
    targetClient <- combineRaceColumnsAndMakeReadable(targetClient)
    targetClient <- makeGenderReadable(targetClient)
    targetClient <- makeEthnicityReadable(targetClient)
    targetClient <- makeVeteranStatusReadable(targetClient)

    pbCounter <- pbCounter + makeHmisCodesReadableIncrement
    setTkProgressBar(pb, pbCounter, label = "Format Homebase Dataframe")

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
                  LastProjectContacted,
                  ActiveInPH
                  FROM targetClient a
                  ")

    pbCounter <- pbCounter + formatHomebaseIncrement
    setTkProgressBar(pb, pbCounter, label = "Homebase Complete")
    close(pb)
    homebase_all
}
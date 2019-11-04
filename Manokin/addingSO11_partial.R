# ----------------- #
# combine SO_11 access db table with the rest of the survey
# some SO_11 data was entered at home and needs to be transferred from SO_11 db to the db with all of the data
# Oct. 2019
# ----------------- #

# ----------------- #
# load packages
# ----------------- #
library(RODBC)
# ----------------- #

# ----------------- #
# upload data
# ----------------- #
# defined db
SO11_db = odbcConnectAccess2007("//orp-dc01/Users/kcoleman/Documents/ManokinFieldDataEntry011.accdb")
db = odbcConnectAccess2007("//orp-dc01/Users/kcoleman/Documents/ManokinFieldDataEntry_CL_10312019/ManokinFieldDataEntry_CL_10312019.mdb")

# see what tables are in there
sqlTables(SO11_db)
sqlTables(db)

# query what you need
SO11_PTSiteData = sqlQuery(SO11_db, paste("select * from PTSiteData"))
SO11_PTRepData = sqlQuery(SO11_db, paste("select * from PTRepData"))
SO11_PTOysData = sqlQuery(SO11_db, paste("select * from PTOysData"))
SO11_SampleIDs = sqlQuery(SO11_db, paste("select * from SampleIDs"))
#SO11_WaterQuality = sqlQuery(SO11_db, paste("select * from WaterQuality")) #empty table - doesnt need to be transferred
SO11_Biomass = sqlQuery(SO11_db, paste("select * from Biomass"))
SO11_Biomass_1 = sqlQuery(SO11_db, paste("select * from Biomass_1"))
SO11_LiveCount_A = sqlQuery(SO11_db, paste("select * from LiveCount_A"))
SO11_LiveCount_B = sqlQuery(SO11_db, paste("select * from LiveCount_B"))
SO11_LiveDensity = sqlQuery(SO11_db, paste("select * from LiveDensity"))
SO11_Scores = sqlQuery(SO11_db, paste("select * from Scores"))
SO11_Scores_1 = sqlQuery(SO11_db, paste("select * from Scores_1"))
odbcClose(SO11_db)

all_PTSiteData = sqlQuery(db, paste("select * from PTSiteData"))
all_PTRepData = sqlQuery(db, paste("select * from PTRepData"))
all_PTOysData = sqlQuery(db, paste("select * from PTOysData"))
all_SampleIDs = sqlQuery(db, paste("select * from SampleIDs"))
all_Biomass = sqlQuery(db, paste("select * from Biomass"))
all_Biomass_1 = sqlQuery(db, paste("select * from Biomass_1"))
all_LiveCount_A = sqlQuery(db, paste("select * from LiveCount_A"))
all_LiveCount_B = sqlQuery(db, paste("select * from LiveCount_B"))
all_LiveDensity = sqlQuery(db, paste("select * from LiveDensity"))
all_Scores = sqlQuery(db, paste("select * from Scores"))
all_Scores_1 = sqlQuery(db, paste("select * from Scores_1"))
# ----------------- #

# ----------------- #
# check data
# ----------------- #
# whats missing
sort(unique(SO11_PTOysData$Rep[!SO11_PTOysData$Rep %in% all_PTOysData$Rep]))
sort(unique(SO11_PTRepData$Rep[!SO11_PTRepData$Rep %in% all_PTRepData$Rep]))

# data unique IDs
## no autonumbers - just SampleEvent - Rep combo

# data classes
all(apply(cbind(as.data.frame(matrix(ncol=1, data = lapply(all_PTSiteData, class))),
                as.data.frame(matrix(ncol=1, data = lapply(SO11_PTSiteData, class)))),
          1, function(x) length(unique(x)) == 1))
all(apply(cbind(as.data.frame(matrix(ncol=1, data = lapply(all_PTRepData, class))), 
                as.data.frame(matrix(ncol=1, data = lapply(SO11_PTRepData, class)))),
          1, function(x) length(unique(x)) == 1))
all(apply(cbind(as.data.frame(matrix(ncol=1, data = lapply(all_PTOysData, class))), 
                as.data.frame(matrix(ncol=1, data = lapply(SO11_PTOysData, class)))),
          1, function(x) length(unique(x)) == 1))
all(apply(cbind(as.data.frame(matrix(ncol=1, data = lapply(all_SampleIDs, class))), 
                as.data.frame(matrix(ncol=1, data = lapply(SO11_SampleIDs, class)))),
          1, function(x) length(unique(x)) == 1))
all(apply(cbind(as.data.frame(matrix(ncol=1, data = lapply(all_Biomass, class))), 
                as.data.frame(matrix(ncol=1, data = lapply(SO11_Biomass, class)))),
          1, function(x) length(unique(x)) == 1))
all(apply(cbind(as.data.frame(matrix(ncol=1, data = lapply(all_Biomass_1, class))), 
                as.data.frame(matrix(ncol=1, data = lapply(SO11_Biomass_1, class)))),
          1, function(x) length(unique(x)) == 1))
all(apply(cbind(as.data.frame(matrix(ncol=1, data = lapply(all_LiveCount_A, class))), 
                as.data.frame(matrix(ncol=1, data = lapply(SO11_LiveCount_A, class)))),
          1, function(x) length(unique(x)) == 1))
all(apply(cbind(as.data.frame(matrix(ncol=1, data = lapply(all_LiveCount_B, class))), 
                as.data.frame(matrix(ncol=1, data = lapply(SO11_LiveCount_B, class)))),
          1, function(x) length(unique(x)) == 1))
all(apply(cbind(as.data.frame(matrix(ncol=1, data = lapply(all_LiveDensity, class))), 
                as.data.frame(matrix(ncol=1, data = lapply(SO11_LiveDensity, class)))),
          1, function(x) length(unique(x)) == 1))
all(apply(cbind(as.data.frame(matrix(ncol=1, data = lapply(all_Scores, class))), 
                as.data.frame(matrix(ncol=1, data = lapply(SO11_Scores, class)))),
          1, function(x) length(unique(x)) == 1))
all(apply(cbind(as.data.frame(matrix(ncol=1, data = lapply(SO11_Scores_1, class))),
                as.data.frame(matrix(ncol=1, data = lapply(all_Scores_1, class)))), 
          1, function(x) length(unique(x)) == 1))
# ----------------- #

# # ----------------- #
# # save data
# # ----------------- #
# sqlSave(db, SO11_PTSiteData, tablename = "PTSiteData", append=TRUE, rownames=FALSE, colnames=FALSE, verbose=FALSE)
# sqlSave(db, SO11_PTRepData, tablename = "PTRepData", append=TRUE, rownames=FALSE, colnames=FALSE, verbose=FALSE)
# sqlSave(db, SO11_PTOysData, tablename = "PTOysData", append=TRUE, rownames=FALSE, colnames=FALSE, verbose=FALSE)
# sqlSave(db, SO11_SampleIDs, tablename = "SampleIDs", append=TRUE, rownames=FALSE, colnames=FALSE, verbose=FALSE)
# sqlSave(db, SO11_Biomass, tablename = "Biomass", append=TRUE, rownames=FALSE, colnames=FALSE, verbose=FALSE)
# sqlSave(db, SO11_Biomass_1, tablename = "Biomass_1", append=TRUE, rownames=FALSE, colnames=FALSE, verbose=FALSE)
# sqlSave(db, SO11_LiveCount_A, tablename = "LiveCount_A", append=TRUE, rownames=FALSE, colnames=FALSE, verbose=FALSE)
# sqlSave(db, SO11_LiveCount_B, tablename = "LiveCount_B", append=TRUE, rownames=FALSE, colnames=FALSE, verbose=FALSE)
# sqlSave(db, SO11_LiveDensity, tablename = "LiveDensity", append=TRUE, rownames=FALSE, colnames=FALSE, verbose=FALSE)
# sqlSave(db, SO11_Scores, tablename = "Scores", append=TRUE, rownames=FALSE, colnames=FALSE, verbose=FALSE)
# sqlSave(db, SO11_Scores_1, tablename = "Scores_1", append=TRUE, rownames=FALSE, colnames=FALSE, verbose=FALSE)
# # ----------------- #
# 
# 
# # ----------------- #
# # check that all was saved
# # ----------------- #
# db <- odbcConnectAccess2007("")
# transects.in.db = sqlQuery(db, paste("select * from ___ where id = ___", id, sep=""))
# # ----------------- #

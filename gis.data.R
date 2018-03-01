library(RODBC)
rhs<-odbcConnect("rhs")

qry<-"SELECT patlst.GLOBAL_PATIENT_KEY
,[COHORT]
,[POSTAL_CODE]
,[x]
,[y]
,[DGP]
,[sub_DGP]
FROM [dbo].[ZTEMP_AY_PAT_LIST] patlst
LEFT JOIN [RHS_2015].[dbo].[LK_UP_GPK_15_16] lkup
ON [GPK2015] = patlst.GLOBAL_PATIENT_KEY
LEFT JOIN [dbo].[2008_2016_patients] pat
ON [GPK2016] = pat.GLOBAL_PATIENT_KEY
LEFT JOIN [dbo].[Singapore_location_complete_2016_Apr] loc
ON [POSTAL_CODE] = [postal]"

gis.data<-sqlQuery(rhs,qry)
dir.create("D:/data/GIS")
saveRDS(gis.data,"D:/data/GIS/gis.data.RDS")
saveRDS(gis.data[,-3],"D:/bdtest/GIS/gis.data.RDS")










# 1 - setup

dir.create("data")
dir.create("scripts")
dir.create("visuals")

download.file(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/840424/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_1.csv",
              destfile = "data/eng_imd19_lsoa.csv")
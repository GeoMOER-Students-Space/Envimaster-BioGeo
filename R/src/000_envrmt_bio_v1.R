### Setup project folders and set root dir

# Set project specific subfolders
cat("#--- set Folders ---#",sep = "\n")
project_folders = c("R/data/org/",                                
                    "R/data/stage2/",
                    "R/data/dendro/",
                    "R/REAVER/REAVER_hyperspace",
                    "R/src"
                    
)

# Automatically set root direcory, folder structure and load libraries
cat("#--- set up Environment  ---#",sep = "\n")
envrmt = createEnvi(root_folder = "~/edu/Envimaster-BioGeo", 
                    folders = project_folders, 
                    path_prefix = "path_", libs = libs,
                    alt_env_id = "COMPUTERNAME", alt_env_value = "PCRZP",
                    alt_env_root_folder = "F:/edu/Envimaster-BioGeo")

cat("#--- use '(file.path(envrmt$...) to set path to folderstructure ---#",sep = "\n")
# set temp directory for raster package
#rasterOptions(tmpdir = envrmt$path_tmp)

rm(libs)
cat(" ",sep = "\n")
cat(" ",sep = "\n")
cat(" ",sep = "\n")


cat("#--- Environment ready ---#",sep = "\n")
cat("###########################",sep = "\n")

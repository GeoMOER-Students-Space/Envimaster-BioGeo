### Setup project folders and set root dir

# Set project specific subfolders
cat("#--- set Folders ---#",sep = "\n")
project_folders = c("R/data/org/",   #R data                 # original data   input          
                    "R/data/stage2/",                        # processed data  output
                    "R/data/privot",
                    "R/REAVER/REAVER_hyperspace", # Functions
                    "R/src",
                    "doc",
                    "lit"
                    
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

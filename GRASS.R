library(rgrass7)
library(tmap)
library(raster)

# Load GRASS ecosystem
WPATH <- Sys.getenv("PATH") 
WPATH1 <- paste("C:\\OSGeo4W64\\bin", WPATH, sep=";") 
Sys.setenv(PATH=WPATH1)
initGRASS("C:/OSGeo4W64/apps/grass/grass-7.2.1", tempdir(), override=TRUE) 

# Set on-disk raster variable
rname <- paste0(getwd(), "/GRASS/CODGiK/M-33-6-C-b-1-3.asc")
# rname <- paste0(getwd(), "/GRASS/GMTED2010N10E060_300/10n060e_20101117_gmted_mea300.tif")

# Set GRASS environment and database location 
# loc <- initGRASS("C:/OSGeo4W64/apps/grass/grass-7.2.1", home=tempdir(), gisDbase="GRASS_TEMP", override=TRUE )

# Import raster to GRASS and set region
execGRASS("r.in.gdal", flags="overwrite", parameters=list(input=rname, output="DEM"))
execGRASS("g.region", parameters=list(raster="DEM") ) 

# Print raster
r1 <- readRAST("DEM")
qtm(r1)

# Calculate surface relief ratio and export to img format
execGRASS("r.neighbors", flags="overwrite", parameters=list(input="DEM", output="mean", 
                                                            method="average", size=as.integer(33)) )
execGRASS("r.neighbors", flags="overwrite", parameters=list(input="DEM", output="min", 
                                                            method="minimum", size=as.integer(33)) )
execGRASS("r.neighbors", flags="overwrite", parameters=list(input="DEM", output="max", 
                                                            method="maximum", size=as.integer(33)) )                 
execGRASS("r.mapcalc", flags="overwrite", parameters=list(expression="srr=(mean - min)/(max - min)") )                   
execGRASS("r.out.gdal", flags="overwrite", parameters=list(input="srr", format="HFA", type="Float64", 
                                        output="srr3.img", nodata=-9999) )

r2 <- readRAST("srr")
qtm(r2)
r3 <- raster("srr3.img")
qtm(r3)

# Clean up temp rasters       
execGRASS("g.remove", parameters=list(rast=c("mean", "min", "max", "srr")) )    

# Clean up GRASS workspace
# unlink(paste(getwd(), "GRASS_TEMP", sep="/"), recursive=TRUE)
# file.remove(paste(getwd(), ".grassrc6", sep="/"))

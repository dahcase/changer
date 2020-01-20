library('changer')
library('data.table')

#for get layer dates
source('~/Documents/code/earth_engine/summarize_functions.R')
sg = readRDS('/media/dan/summary_grid.rds')
thecity = 'Lusaka'
out.dir = "/media/dan/processed/"
layerfolder = '/media/dan/earth_engine/'


#Load a dataset and its dates
metadata = sg[funk == "" & time == "" & product == 'MOD13A1' & variables == 'NDVI' & city ==thecity]
rasname = paste0(metadata[, paste(..thecity,product,version,variables,year_start,year_end,sep='_')],'.tif')
ras = readAll(brick(file.path(out.dir,metadata[,prefix], 'latlong', rasname)))
namepath = file.path(layerfolder, paste0('MODIS',ifelse(nchar(metadata[,version])>0, paste0('_',metadata[,version],'_'), "_"), metadata[,product],'.txt'))
nnn = read.delim(namepath, header = F, stringsAsFactors = F)[,1]

if(!all(grepl('_', nnn, fixed = T))){
  nnn = paste(substr(nnn, 1,4), substr(nnn, 5,6), substr(nnn, 7,8), sep = '_')
}
#convert to date paths
nnn = as.Date(nnn, '%Y_%m_%d')

tras = brick(lapply(seq_len(length(nnn)), function(x) raster(matrix(NA, 1,1))))
tras[] = ras[1,1]

res <- changer(tras, nnn)

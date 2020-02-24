library('changer')
library('data.table')

#for get layer dates
source('~/Documents/code/earth_engine/summarize_functions.R')
sg = readRDS('/media/dan/summary_grid.rds')
thecity = 'Dakar'
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

#
ras_sub = crop(ras, extent(ras, 1, 2, 1, 2))
ras_sub_sub = crop(ras, extent(ras,1,1,1,1))
res0 = changer(ras_sub, nnn)
res1 = changer(ras_sub_sub, nnn)
# res1 = changer(ras_sub, nnn, changepoint.range = 1)
# res2 = changer(ras_sub, nnn, changepoints = res1[2,2][[1]]$change_time[c(1,5,10,15,20,25)])
# res3 = changer(ras_sub, nnn, changepoint.prior.scale = .005)
# res4 = changer(ras_sub, nnn, changepoint.prior.scale = .5)


r1 <- changer(ras, nnn, changepoint.range = .95, changepoint.prior.scale = .05)
r2 <- changer(ras, nnn, changepoint.range = .95, changepoint.prior.scale = .5)
r3 <- changer(ras, nnn, changepoint.range = .95, changepoint.prior.scale = .005)

#create raster bricks from the change points


#
# tras = brick(lapply(seq_len(length(nnn)), function(x) raster(matrix(NA, 1,1))))
# tras[] = ras[1,1]
#
# df = data.frame(ds = nnn, y = as.numeric(tras[]))
# mod = prophet::prophet(df)
#
#
#
#
#
# res0 = changer(ras_sub, nnn)
# res1 <- changer(tras, nnn)
# res2 <- changer(tras, nnn, n.changepoints = 100)
#
#
#
#
# #changeypointy
#
# chg1 = lapply(c(1,10,100,200), function(x) changer(tras,nnn,n.changepoints = x)[[1]]$changepoints)
# vapply(chg1, length, 1) #same as input?
#
# chg2 = lapply(c(1,10,100,200), function(x) changer(tras,nnn,n.changepoints = x, changepoint.prior.scale = .01)[[1]]$changepoints)
# vapply(chg2, length, 1) #same as input?
#
# chg3 = lapply(c(1,10,100,200), function(x) changer(tras,nnn,n.changepoints = x, changepoint.prior.scale = .001)[[1]]$changepoints)
# vapply(chg3, length, 1) #same as input?
#
#
# blah = changer(tras, nnn, n.changepoints = 100, changepoint.prior.scale = .01)[[1]]
# tras10000 = tras*10000
# blah = changer(tras10000, nnn, n.changepoints = 10, changepoint.prior.scale = .5)[[1]]


library('changer')
library('data.table')

#for get layer dates
source('~/Documents/code/earth_engine/summarize_functions.R')
sg = readRDS('/media/dan/summary_grid.rds')
thecity = 'Ouagadougou'
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


# res1 = changer(ras_sub, nnn, changepoint.range = 1)
# res2 = changer(ras_sub, nnn, changepoints = res1[2,2][[1]]$change_time[c(1,5,10,15,20,25)])
# res3 = changer(ras_sub, nnn, changepoint.prior.scale = .005)
# res4 = changer(ras_sub, nnn, changepoint.prior.scale = .5)


t_ras = crop(ras, extent(ras,40,45,45,50))
t_1 = changer(t_ras, nnn, changepoint.range = .95, changepoint.prior.scale = .05)
t_1_arr = as.array(t_1)
stopifnot(ncol(t_ras)==ncol(t_1))
stopifnot(nrow(t_ras) == nrow(t_1))

chglocs = as.Date(substr(names(t_1), 2, nchar(names(t_1))), format = '%Y.%m.%d')
t_arr = as.array(t_ras)
t_2 = apply(t_arr, 1:2, pixel_prophet, dates = nnn, dots = list(changepoints = chglocs, changepoint.range = .95, changepoint.prior.scale = .05))
t_2 = aperm(t_2, c(2,3,1))

all.equal(t_1_arr, t_2)

r1 <- readAll(changer(ras, nnn, changepoint.range = .95, changepoint.prior.scale = .05))
r1_arr = apply(as.array(ras), 1:2, function(x) pixel_prophet(x, dates = nnn, dots = list(changepoints = chglocs, changepoint.range = .95, changepoint.prior.scale = .05)))
r1_arr = aperm(r1_arr, c(2,3,1))

b = all.equal(r1_arr,as.array(r1))



r2 <- readAll(changer(ras, nnn, changepoint.range = .95, changepoint.prior.scale = .5))
r3 <- readAll(changer(ras, nnn, changepoint.range = .95, changepoint.prior.scale = .005))


validate_results = function(base_ras, res, npts = 100){

  crds = expand.grid(1:ncol(base_ras), 1:nrow(base_ras))
  sample_rows = sample(1:nrow(crds), npts, replace = F)

  chglocs = as.Date(substr(names(res), 2, nchar(names(res))), format = '%Y.%m.%d')

  #at the sample points, run changer
  samps = lapply(sample_rows, function(x){
    col = crds[x,1]
    row = crds[x,2]

    sras = c(base_ras[row,col])

    compare = c(res[row,col])

    chg = pixel_prophet(sras, nnn, list(changepoint.range = .95, changepoint.prior.scale = .05, changepoints = chglocs))

    return(list(sum(chg == compare), chg, compare))

  })

  return(samps)

}

blah = validate_results(t_ras, t_1, 10)




# test_pairs = data.table(sample(1:102, 10, replace = F), sample(1:110, 10, replace = F))

# tests = lapply(1:nrow(test_pairs), function(x){
#   rsub = crop(ras, extent(ras, test_pairs[x,V1],test_pairs[x,V1],test_pairs[x,V2],test_pairs[x,V2]))
#   res = changer(rsub, nnn, changepoint.range = .95, changepoint.prior.scale = .05)
#   chk = crop(r1, extent(r1, test_pairs[x,V1],test_pairs[x,V1],test_pairs[x,V2],test_pairs[x,V2]))
#
#   return(list(res,chk))
# })
#
# tchk = vapply(tests, function(x) isTRUE(all.equal(x[[1]][], x[[2]][])), T)
#
#
# #testy
# test_ras = crop(ras, extent(ras, 10,20,10,20))
# tr_chg = changer(test_ras, nnn, changepoint.range = .95, changepoint.prior.scale = .05)
# tps = setDT(expand.grid(V1 = seq(10:20), V2 = seq(10:20)))
# o <- lapply(1:nrow(tps), function(x){
#   rsub = crop(test_ras, extent(ras, tps[x,V1] + 9,tps[x,V1],tps[x,V2],tps[x,V2]))
#   res = changer(rsub, nnn, changepoint.range = .95, changepoint.prior.scale = .05)
#   chk = crop(tr_chg, extent(tr_chg, tps[x,V1],tps[x,V1],tps[x,V2],tps[x,V2]))
#   chk2 = crop(tr_chg, rsub)
#
#   return(list(res,chk, chk2))
# })
#
# chker = lapply(o, function(x) isTRUE(all.equal(x[[1]][], x[[2]][])))
# table(unlist(chker))


#create summaries
make_summaries = function(x){

  #big change
  x_bin = abs(x)>.001
  x_sum = sum(x_bin)
  x_abs_change = sum(abs(x))
  x_abs_max = max(abs(x))
  x_min = min(x)
  x_max = max(x)

  r = brick(list(x_sum, x_abs_change, x_abs_max, x_min, x_max))
  names(r) = c('binned_sum', 'sum_abs_chg', 'max_abs_chg', 'min_chg', 'max_chg')
  return(r)

}

r1_sum = make_summaries(r1)
r2_sum = make_summaries(r2)
r3_sum = make_summaries(r3)

writeRaster(r1, filename = '/media/dan/changer/r1.tif', overwrite = T)
writeRaster(r2, filename = '/media/dan/changer/r2.tif', overwrite = T)
writeRaster(r3, filename = '/media/dan/changer/r3.tif', overwrite = T)


writeRaster(r1_sum, filename = '/media/dan/changer/r1_sum.tif', overwrite = T)
writeRaster(r2_sum, filename = '/media/dan/changer/r2_sum.tif', overwrite = T)
writeRaster(r3_sum, filename = '/media/dan/changer/r3_sum.tif', overwrite = T)

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


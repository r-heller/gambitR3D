args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[grepl('^--file=',args)])
root <- dirname(dirname(normalizePath(script[1])))
output <- tempfile('hex-render-')
status <- system2(file.path(R.home('bin'),'Rscript'), c('--vanilla',shQuote(file.path(root,'tools/branding/render-personal.R')),shQuote(output),'gambitR3D'))
if(status!=0L) stop('Logo rendering failed')
map <- read.csv(file.path(root,'tools/branding/package-assets.csv'),stringsAsFactors=FALSE)
for(i in seq_len(nrow(map))) {
 target <- file.path(root,map$destination[i]);dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE)
 if(!file.copy(file.path(output,'gambitR3D',map$rendered[i]),target,overwrite=TRUE)) stop('Could not copy logo')
}

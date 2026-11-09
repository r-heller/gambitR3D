args <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',args[grepl('^--file=',args)])
root <- dirname(normalizePath(script[1]))
fontconf <- tempfile(fileext='.conf')
writeLines(c('<?xml version="1.0"?><!DOCTYPE fontconfig SYSTEM "fonts.dtd"><fontconfig>',
 '<include ignore_missing="yes">/etc/fonts/fonts.conf</include>',
 '<include ignore_missing="yes">/opt/homebrew/etc/fonts/fonts.conf</include>',
 paste0('<dir>',file.path(root,'fonts'),'</dir></fontconfig>')),fontconf)
Sys.setenv(FONTCONFIG_FILE=fontconf)
source(file.path(root,'R','cttir_hex.R'))
source(file.path(root,'personal-motifs.R'))
cttir_motif <- personal_motif
cttir_accents <- c(cttir_accents,games='#7956B5',embedding='#146554',creative='#91617B')
params <- commandArgs(trailingOnly=TRUE)
output <- if(length(params)) params[1] else file.path(root,'rendered')
names <- c('gambitR','gambitR3D','diplomacyR','maumauR','multixoR','risikoR','skatR','tesseractR','foldbenchR','synesthR')
if(length(params)>1L) {stopifnot(params[2] %in% names); names <- params[2]}
for(name in names) {
 family <- if(name=='foldbenchR') 'embedding' else if(name=='synesthR') 'creative' else 'games'
 cttir_render(name,family,file.path(output,name),width=1200L)
 cat(name,family,'\n')
}

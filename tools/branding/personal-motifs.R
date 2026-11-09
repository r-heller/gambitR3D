# Package-specific motifs using the CTTIR v5 vector primitives.
personal_motif <- function(name, s, small=FALSE) {
  list2env(cttir_primitives(s, small), envir=environment())
  a <- s$mid; p <- s$paper; l <- s$light; d <- s$deep; e <- s$edge
  ink <- if(s$mode=="dark") s$light else s$deep
  cross <- function(x,y,r=.07) { L(x+c(-r,r),y+c(-r,r),ink,5); L(x+c(-r,r),y+c(r,-r),ink,5) }
  ring <- function(x,y,r=.06) C(x,y,r,fill=NA,col=ink,lwd=5)
  pawn <- function(x,y,k=1) { C(x,y+.13*k,.045*k,p,shadow=TRUE); P(x+c(-.035,.035,.065,-.065)*k,y+c(.09,.09,-.04,-.04)*k,a,shadow=TRUE); B(x,y-.055*k,.16*k,.035*k,d,.01) }
  board <- function(x,y,w=.28) { B(x,y,w,w,l,.018,shadow=TRUE); for(t in c(-1,1)/6) {L(x+c(-w/2,w/2),y+t*w+c(0,0),e,2);L(x+t*w+c(0,0),y+c(-w/2,w/2),e,2)} }
  heart <- function(x,y,r=.07,fill=d) { t<-seq(0,2*pi,length.out=80); P(x+r*sin(t)^3,y+r*(13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t))/16,fill) }
  knight <- function(x=.5,y=.50,k=1) { P(x+c(-.17,-.12,-.08,-.11,-.02,.05,.08,.18,.18,.10,.07,-.02,-.09,-.17)*k,y+c(-.25,-.08,.04,.15,.25,.27,.34,.18,.02,-.03,-.17,-.21,-.25,-.25)*k,a,col=e,lwd=2,shadow=TRUE); C(x+.065*k,y+.14*k,.012*k,d); L(x+c(.04,.105)*k,y+c(.02,.06)*k,l,3);B(x,y-.27*k,.39*k,.055*k,d,.015) }
  if(name=='gambitR') { slab(.5,.25,.70,.24,.055,l,grid=TRUE); knight(.5,.55,.85)
  } else if(name=='gambitR3D') { slab(.5,.26,.78,.25,.04,d,grid=TRUE);slab(.5,.43,.70,.24,.04,l,grid=TRUE);knight(.5,.68,.65)
  } else if(name=='diplomacyR') { L(c(.23,.5,.77),c(.53,.72,.53),e,4); L(c(.23,.77),c(.48,.48),e,4);pawn(.23,.51,.8);pawn(.77,.51,.8);pawn(.5,.70,.8);B(.5,.30,.35,.22,p,.025,shadow=TRUE);L(c(.38,.62),c(.34,.34),d,3);L(c(.39,.58),c(.28,.28),e,2);C(.62,.22,.04,a)
  } else if(name=='maumauR') { B(.39,.53,.36,.51,l,.035,shadow=TRUE);B(.60,.48,.36,.51,p,.035,shadow=TRUE);heart(.60,.49,.10);C(.28,.71,.025,d);A(.49,.51,.35,205,340,e,5);P(c(.82,.87,.78),c(.30,.41,.39),e)
  } else if(name=='skatR') { B(.28,.53,.29,.48,l,.025,shadow=TRUE);B(.50,.49,.29,.48,a,.025,shadow=TRUE);B(.72,.45,.29,.48,p,.025,shadow=TRUE);P(c(.28,.34,.28,.22),c(.65,.56,.47,.56),d);C(.47,.54,.035,d);C(.53,.54,.035,d);C(.50,.59,.035,d);L(c(.50,.50),c(.54,.45),d,4);heart(.72,.46,.07)
  } else if(name=='risikoR') { C(.5,.56,.28,l,col=e,lwd=2,shadow=TRUE);P(c(.28,.36,.43,.39,.44,.35,.31),c(.67,.76,.70,.61,.55,.50,.58),a);P(c(.51,.63,.74,.69,.61,.57),c(.74,.78,.62,.57,.61,.52),d);P(c(.58,.65,.69,.61),c(.47,.49,.35,.34),a);L(c(.32,.60,.74),c(.39,.31,.47),e,3,lty=2);pawn(.30,.25,.6);pawn(.68,.24,.6)
  } else if(name=='multixoR') { L(c(.29,.50,.73),c(.70,.48,.70),e,5);L(c(.50,.50),c(.48,.25),e,5);board(.28,.70,.29);board(.73,.70,.29);board(.50,.28,.29);cross(.28,.70,.055);ring(.73,.70,.055);cross(.50,.28,.055)
  } else if(name=='tesseractR') { x<-c(.20,.62,.62,.20,.20);y<-c(.25,.25,.67,.67,.25);L(x,y,ink,5);L(x+.17,y+.15,a,5);for(i in 1:4)L(c(x[i],x[i]+.17),c(y[i],y[i]+.15),e,3);cross(.39,.45,.07);ring(.66,.69,.045)
  } else if(name=='foldbenchR') { for(i in 0:3){x<-.17+i*.165;P(c(x,x+.165,x+.165,x),c(.34,.46,.75,.63),if(i%%2) a else l,col=e,lwd=2,shadow=TRUE)};L(c(.17,.335,.50,.665,.83),c(.34,.46,.34,.46,.34),d,3);L(c(.19,.82),c(.21,.21),ink,2,arrow=TRUE)
  } else if(name=='synesthR') { B(.29,.56,.32,.48,p,.025,shadow=TRUE);for(i in 0:3)L(c(.19,.38),rep(.72-i*.08,2),e,3);t<-seq(0,1,length.out=90);L(.40+.43*t,.49+.10*sin(4*pi*t),a,5);L(c(.70,.70,.84,.84),c(.58,.79,.82,.64),ink,5);C(.65,.57,.052,ink);C(.79,.63,.052,ink)
  } else stop('No personal motif: ',name)
}

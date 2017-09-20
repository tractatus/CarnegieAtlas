folder<-'/Users/danielfurth/Downloads/FetalHeart_RoiSet'

ontology<-read.table('~/Downloads/heart_ontology - Sheet1.csv', sep=',', header=TRUE, fill = TRUE
)

files<-dir(folder, full.names=TRUE)
#read.ijroi(files)

roi_name<-strsplit(tools::file_path_sans_ext(basename(files)), '_')

roi_name <-do.call('rbind', roi_name)
roi_name <- data.frame(roi_name)
roi_name[,1]<-as.numeric(as.vector(roi_name[,1]))
roi_name$ID<-1:nrow(roi_name)
roi_name <-roi_name[with(roi_name, order(X2, X1)), ]

library(wholebrain)
images<-get.images('/Users/danielfurth/Documents/acadmic/scilifelab/heart_atlas_png', 'png')

R<-array(data = rep(0, prod(c(598, 532, length(images)))), dim = c(598, 532, length(images)))
G<-array(data = rep(0, prod(c(598, 532, length(images)))), dim = c(598, 532, length(images)))
B<-array(data = rep(0, prod(c(598, 532, length(images)))), dim = c(598, 532, length(images)))

for(i in seq_along(images)){
  img <- readPNG(images[i], FALSE)
  R[,,i]<-as.matrix(img[,,1])
  G[,,i]<-as.matrix(img[,,2])
  B[,,i]<-as.matrix(img[,,3])
  
  cat(paste('\r','Iter:',i))
}
R<-R*255
G<-G*255
B<-B*255

regions.to.be.included<-ontology[ontology$acronym%in%unique(roi_name[,2]),]

for(i in seq_along(images)){
  img <- readPNG(images[i], FALSE)
  R[,,i]<-as.matrix(img[,,1])
  G[,,i]<-as.matrix(img[,,2])
  B[,,i]<-as.matrix(img[,,3])
  
  cat(paste('\r','Iter:',i))
}


r<-regions.to.be.included$r[5]
g<-regions.to.be.included$g[5]
b<-regions.to.be.included$b[5]
for(i in seq_along(images)){
	mask<-((R[,,331]==r)&(G[,,331]==g)&(B[,,331])+0)


}

p.roi<-read.ijroi(files[roi_name[roi_name$X1==331,]$ID])


        par(xaxs='r', yaxs='r')
        plot(c(0, dim(img)[2]),c(0, dim(img)[1]), axes=F, asp=1, col=0, xlab='', ylab='', ylim=c(dim(img)[1],0))

        rasterImage(mask,0,0, dim(img)[2], dim(img)[1])
        
        polygon(p.roi$coords, border='red')
        
        points.with.color<-which(mask==1, arr.ind=TRUE)
        
        pip.test<-which(point.in.polygon(points.with.color[,1], points.with.color[,2], p.roi$coords[,1], p.roi$coords[,2])==0)
        
        mask[points.with.color[pip.test,]]<-0
        
#Outflow track allt ting annat är uppmärkt.
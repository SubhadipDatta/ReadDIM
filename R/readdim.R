#' Import ESA SNAP .dim file
#' @author  Subhadip Datta
#' @param dim_path Path to the .dim file.
#' @param noDataV Value of no data used in raster.
#' @param stacked If true create a stacked image else list
#' @import sp
#' @import raster
#' @import rgeos
#' @import rgdal
#' @import stringr
#' @examples
#' library(raster)
#' library(ReadDIM)
#' im<-system.file("exd","td.dim",package = "ReadDIM")
#' read.dim(im)
#' @export
read.dim<-function(dim_path,noDataV=NA,stacked=FALSE){
    f<-gsub(".dim",".data",dim_path)
    fil<-str_sort(list.files(f,pattern = "*.img$",full.names = TRUE),numeric = T)
    rasl<-c()
    for (i in seq(1,length(fil),1)){
      ras<-suppressWarnings(raster(fil[i]))
      if(is.na(noDataV)==FALSE){
        ras[ras==noDataV]<-NA
      } else {
        ras<-setMinMax(ras)
      }
      if(as.character(crs(ras))=="+proj=longlat +ellps=WGS84 +no_defs"){
        crs(ras)<-CRS("+proj=longlat +datum=WGS84 +no_defs")
        rasl<-append(rasl,ras)
      } else{
        rasl<-append(rasl,ras)
      }
    }
    if(stacked==FALSE){
      return(rasl)
    } else {
      rasl<-stack(rasl)
      return(rasl)
    }
}

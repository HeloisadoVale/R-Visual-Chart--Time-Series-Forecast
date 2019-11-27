source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("ggplot2");
libraryRequireInstall("plotly")
####################################################

################### Actual code ####################
library(ggplot2)
"X<-data.frame(mpg$cty)
Y<-data.frame(mpg$hwy) 
Legend<-data.frame(mpg$cyl) 
Column<-data.frame(mpg$drv)"
#Row<-data.frame(mpg$year)


 if(exists( "Row")==TRUE && exists("Column")==TRUE){
if(length(X)!=0 && length(Y)!=0&& length(Legend)!=0&&length(Column)!=0 && Row!=0 )
  Values<-data.frame(X,Y,Legend,Column,Row)
 }
if(exists( "Row")==FALSE && exists("Column")==TRUE){
if(length(X)!=0 && length(Y)!=0&& length(Legend)!=0&&length(Column)!=0  )
Values<-data.frame(X,Y,Legend,Column)
}
if(exists( "Row")==TRUE && exists("Column")==FALSE){
if(length(X)!=0 && length(Y)!=0&& length(Legend)!=0&& length(Row)!=0 )
Values<-data.frame(X,Y,Legend,Row)
}


#ChartType="Scatter"
#Values=data.frame(mpg$cty,mpg$hwy,mpg$cyl,mpg$drv,mpg$year)
#if(length(X)!=0 && length(Y)!=0&& length(Legend)!=0&&length(Column)!=0 && length(Row)!=0 )
  #Values=5
#"g=0
#ncol(Values)==5"
if(exists("ChartType")==FALSE)ChartType<-"Scatter"
if( ncol(Values)==5 && ChartType=="Scatter")
{
names(Values)[4]<-paste("W")
names(Values)[5]<-paste("M")
g=ggplot(Values,aes_string(names(Values[1]),names(Values[2]),color=names(Values[3])))+geom_jitter()+facet_grid( M ~  W,scales = "free")

}

if(ncol(Values)==5 && ChartType=="BarChart")
{
  names(Values)[4]<-paste("W")
  names(Values)[5]<-paste("M")
  g=ggplot(Values,aes_string(names(Values[1]),names(Values[2]),fill=names(Values[3])))+geom_bar(stat="identity")+facet_grid( M ~  W,scales = "free")
}
if(ncol(Values)==5 && ChartType=="BoxPlot")
{
  names(Values)[4]<-paste("W")
  names(Values)[5]<-paste("M")
  g=ggplot(Values,aes_string(names(Values[1]),names(Values[2]),fill=factor(names(Values[3]))))+geom_boxplot()+facet_grid( M ~  W,scales = "free")
}
if(ncol(Values)==5 && ChartType=="PieChart")
{ 
  names(Values)[4]<-paste("W")
  names(Values)[5]<-paste("M")
  g=ggplot(Values,aes_string(names(Values[1]),names(Values[2]),fill=names(Values[3])))+geom_bar(stat="identity")+coord_polar("x")+facet_grid( M ~  W,scales = "free")
}

if(ncol(Values)==4&&ChartType=="Scatter")
{
  names(Values)[4]<-paste("W")
  g=ggplot(Values,aes_string(names(Values[1]),names(Values[2]),color=names(Values[3])))+geom_jitter()+facet_grid( W~.)
}
if(ncol(Values)==4&&ChartType=="BarChart")
{
  names(Values)[4]<-paste("W")
  g=ggplot(Values,aes_string(names(Values[1]),names(Values[2]),fill=names(Values[3])))+geom_bar(stat="identity")+facet_grid( W~.)
}
if(ncol(Values)==4&&ChartType=="BoxPlot")
{
  names(Values)[4]<-paste("W")
  g=ggplot(Values,aes_string(names(Values[1]),names(Values[2]),fill=factor(names(Values[3]))))+geom_boxplot()+facet_grid( W~.)
}

if(ncol(Values)==4&&ChartType=="PieChart")
{
  names(Values)[4]<-paste("W")
  g=ggplot(Values,aes_string(names(Values[1]),names(Values[2]),fill=names(Values[3])))+geom_bar(stat="identity")+coord_polar("x")+facet_grid( W~.)
}

####################################################

############# Create and save widget ###############
p = ggplotly(g);
internalSaveWidget(p, 'out.html');
####################################################

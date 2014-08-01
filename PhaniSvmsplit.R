#A data frame or data table with Lat and Long must be passed
svmsplit<-function(input){
   if(is.data.frame(input)) { dataf = input}
   else if(is.list(input) & length(list)==1) {dataf = input[[1]]}
   else if(class(input)=="Leaf") { dataf = input@value}
   else stop("Not dataframe or list or Leaf")
   km<-kmeans(dataf,centers=2,nstart=1000)
   library(e1071)
   svmdata<-cbind(dataf[,1],dataf[,2],km$cluster)
   svmdata<-as.data.frame(svmdata)
   names(svmdata)<-c("Lat","Long","kmclust")
   svmdata[,3]<-as.factor(svmdata[,3])
   svmfit<-svm(kmclust~.,data=svmdata,kernel="radial")
   #plot(svmfit,svmdata)
   svmfit
   pred<-predict(svmfit,newdata=dataf)
   clusters.list <- split(dataf, pred)
}

phanisvmsplit<-function(leaf, numc, maxcs, mincs){
   if(numc > 0 & nrow(leaf@value) > mincs){
      
      # Initialize each recursion
      temp <- svmsplit(leaf)
      # Ensure that if split clusters are smaller than mincs then that split will not take place
      if(nrow(temp[[1]])>mincs & nrow(temp[[2]])>mincs){
         tree <- createChilds(leaf, temp)
         numL <- nrow(tree@left@value)*numc/nrow(tree@value)
         numR <- nrow(tree@right@value)*numc/nrow(tree@value)
         
         ##### Recursion begin
         a<-runif(1,-1,1)
         if(a>=0){
            #numL <- numc - leafCount(tree)
            treeLeft <- phanisvmsplit(tree@left, numL, maxcs, mincs)
            #numR <- numL - leafCount(treeLeft)
            treeRight <- phanisvmsplit(tree@right, numR, maxcs, mincs)
         } else {
            #numR <- numc - leafCount(tree)
            treeRight <- phanisvmsplit(tree@right, numR, maxcs, mincs)
            #numL <- numR - leafCount(treeRight)
            treeLeft <- phanisvmsplit(tree@left, numL, maxcs, mincs)
         }
         ##### Recursion end
         
         # Join recursion results
         tree <- createChilds(tree, treeLeft, treeRight)
      }
   }
   i <- i+1
   if (exists("tree")){
      invisible(tree)
   } else {
      invisible(leaf)
   }
}

myTree<-phanisvmsplit(CreateLeaf(storeData.df), 30, 6, 20)
myTreeCList<-leafList(myTree)
count <- leafCount(myTree)
print(count)
myTreeCdfList<-leafListCdf(myTreeCList)

bigCList<-bigClustsLeaf(myTree, 70)
for(i in 1:length(bigCList)){
   if(!exists("out")){
      out<-svmsplit(bigCList[[i]])
   } else {
      out <- c(out, svmsplit(bigCList[[i]]))
   }
}



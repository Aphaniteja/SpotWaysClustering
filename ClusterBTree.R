storeData.df <- read.csv("storeData.df")
setClassUnion("listOrDataFrame", c("list", "data.frame"))
# Creating a simple Binary Tree class
setClass("Leaf", slots = c(value = "data.frame", ID = "numeric"))
setClass("Node", slots = c(left = "ANY", right = "ANY"),
         prototype = prototype(left = new("Leaf"), right = new("Leaf")), 
         contains = "Leaf")
setClassUnion("leafOrNode", c("Leaf", "Node"))
setGeneric("CreateLeaf", function(val) standardGeneric("CreateLeaf"))
setMethod(CreateLeaf, "ANY", function(val) {
   out <- 0
   if(class(val)=="Leaf") {out <- val}
   else if(is.data.frame(val)){ out <- new("Leaf", value = val) }
   else if(is.list(val) & length(val)==1){ out <- new("Leaf", value = val[[1]]) }
   else { 
      print(class(val))
      stop("Value is neither data.frame nor list nor Leaf") }
   
   invisible(out)
})
# Create a node with children as nodes
setGeneric("CreateNode", function(node, left, right) standardGeneric("CreateNode"))
setMethod("CreateNode", signature(node = "Node", left = "leafOrNode", right = "leafOrNode"), function(node, left, right) {
   out<-new("Node", value = node@value, left = left, right = right)
})
# Create a node with value of leaf as value and children as two leaves with values left and right
setGeneric("createChilds", function(leaf, left, right) standardGeneric("createChilds"))
setMethod(createChilds, signature(leaf = "Leaf", left = "Leaf", right = "Leaf"), function(leaf, left, right){
   l <- CreateLeaf(left)
   r <- CreateLeaf(right)
   if(nrow(l@value) > nrow(r@value)){
      temp <- l
      l <- r
      r <- temp
   }
   out<- new("Node", value = leaf@value, left = l, right = r)
})
setMethod(createChilds, signature(leaf = "Leaf", left = "listOrDataFrame", right = "listOrDataFrame"), function(leaf, left, right){
   l <- CreateLeaf(left)
   r <- CreateLeaf(right)
   if(nrow(l@value) > nrow(r@value)){
      temp <- l
      l <- r
      r <- temp
   }
   out<- new("Node", value = leaf@value, left = l, right = r)
   invisible(out)
})
setMethod(createChilds, signature(leaf = "Leaf", left = "list", right = "missing"), function(leaf, left, right){
   stopifnot(length(left)==2)
   if(class(left[[1]])=="Node" || class(left[[2]])=="Node") browser(text = "Sending node to createLeaf")
   out <- createChilds(leaf, left[[1]], left[[2]])
   
   invisible(out)
})
setMethod(createChilds, signature(leaf = "Leaf", left = "missing", right = "list"), function(leaf, left, right){
   stopifnot(length(right)==2)
   out <- createChilds(leaf, left = right)
   
   invisible(out)
})

setMethod(createChilds, signature(leaf = "Node", left = "leafOrNode", right = "leafOrNode"), function(leaf, left, right){
   if(nrow(left@value) > nrow(right@value)){
      temp <- left
      left <- right
      right <- temp
   }
   invisible(CreateNode(leaf, left, right))
})

leafList<-function(leaf){
   count<-0
   outList<-list()
   if(!(class(leaf)=="Leaf" || class(leaf)=="Node")) { stop("Tree is neither Leaf nor Node") }
   if(class(leaf)=="Leaf"){
      df<-leaf@value
      if (identical(outList, c(outList, df))) browser()
      outList <- c(outList, df)
   } else {
      l1<-leafList(leaf@left)
      l2<-leafList(leaf@right)
      outList <- c(l1, l2)
   }
}
leafListCdf<-function(outList){
   out<-list()
   for(i in 1:(length(outList)/2)){
      index = 2*i
      out[[i]]<- data.frame(outList[index - 1], outList[index])
      out[[i]]$clust<-i
   }
   numClust<-length(out)
   out<-do.call("rbind", out)
   print(table(out[,3]))
   if(numClust<25){
      plot(out[,1],out[,2], col = out[,3], pch = out[,3])
   } else {
      plot(out[,1], out[,2], col = out[,3], cex = 0)
      text(out[,1], out[,2], out[,3], col = out[,3])
   }
   invisible(out)
   
}
leafCount<-function(leaf){
   count <- 0
   if(!(class(leaf)=="Leaf" || class(leaf)=="Node")) { stop("Tree is neither Leaf nor Node") }
   if(class(leaf)=="Leaf"){
      count <- count + 1
   } else {
      count <- leafCount(leaf@left) + leafCount(leaf@right)
   }
   invisible(count)
}

bigClustsLeaf <- function(input, maxcs){
   out<-list()
   if(class(input)=="Leaf"){
      if(nrow(input@value)>maxcs){
         out<-c(out, input)
      }
   } else {
      l1<-bigClustsLeaf(input@left, maxcs)
      l2<-bigClustsLeaf(input@right, maxcs)
      if(length(l1)!=0){
         if(length(l2)!=0){
            out<-c(l1,l2)
         } else {
            out<-l1
         }
      } else {
         if(length(l2)!=0){
            out<-l2
         } else {
            
         }
      }
   }
}






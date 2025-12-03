setwd("~/portfolio/AoC_2023/day_20/")
data <- read.delim("dummy_1.txt", header=FALSE, sep="-")

#wrangle into list of nodes and destinations
data$V2 <- gsub(">", "", data$V2)
data$V2 <- gsub(" ", "", data$V2)
data$V1 <- gsub(" ", "", data$V1)

#node children
nodes <- strsplit(data$V2, split = ",")
names(nodes) <- substr(data$V1, start = 2, stop = nchar(data$V1))
#node type
node_type <- substr(data$V1, start = 1, stop = 1)
names(node_type) <- names(nodes)

#initialize data frame with all the info
#node, type, in, out, status
nodes_panel <- data.frame(node = names(nodes), type = node_type,
                          inp = rep("low", length(nodes)),
                          outp = rep("low", length(nodes)),
                          status = ifelse(node_type == "%", "0", "1"))
#find all the conj nodes and list their inputs
conj_status <- list()
for(node in names(node_type)[which(node_type == "&")]) {
  incoming <- names(nodes)[lapply(nodes, function(x) {node %in% x}) |> unlist()]
  conj_status <- c(conj_status, list(incoming))
}
names(conj_status) <- names(node_type)[which(node_type == "&")]

#actually solve the problem
COUNTER <- c()
for(z in 1:1000) {
  #start at broadcaster
  nodes_to_visit <- nodes["roadcaster"] |> unlist()
  COUNTER <- c(COUNTER, "low", rep("low", length(nodes_to_visit)))
  while(length(nodes_to_visit) > 0) {
    next_nodes_to_visit <- c()
    nodes_panel_update <- nodes_panel
    for(n in nodes_to_visit) {
      #if the node does not have any children skip
      if(n %in% setdiff(unlist(nodes) |> unique(), names(nodes))) next
      type <- nodes_panel[n, "type"]
      #decide what happens based on the type
      if(type == "%") {
        #if pulse received is not high - update out and on/off status
        r <- nodes_panel[n, "inp"]
        if(r != "high") {
          #switch to the opposite 
          nodes_panel_update[n, "status"] <- ifelse(nodes_panel[n, "status"] == 0, 1, 0)
          #update out
          nodes_panel_update[n, "outp"] <- ifelse(nodes_panel_update[n, "status"] == 0, "low", "high")
          #print(paste(n, "-", nodes_panel_update[n, "outp"], "to", unlist(nodes[n])))
          #use this out to update the input to all the children
          nodes_panel_update[(nodes_panel$node %in% unlist(nodes[n])), "inp"] <- nodes_panel_update[n, "outp"]
          #count
          COUNTER <- c(COUNTER, rep(nodes_panel_update[n, "outp"], length(unlist(nodes[n]))))
          #update what's next
          next_nodes_to_visit <- c(next_nodes_to_visit, unlist(nodes[n]))
        }
      } else {
        #if dealing with a conj node, look into its history
        statuses <- nodes_panel_update[(nodes_panel$node %in% conj_status[[n]]), "outp"]
        #count all high
        count <- sum(statuses == "high")
        #update out
        if(length(statuses) > 1) {
          nodes_panel_update[n, "outp"] <- ifelse(count == length(statuses), "low", "high")
        } else {
          nodes_panel_update[n, "outp"] <- ifelse(statuses == "low", "high", "low")
        }
          
        #use this out to update the input to all the children
        nodes_panel_update[(nodes_panel$node %in% unlist(nodes[n])), "inp"] <- nodes_panel_update[n, "outp"]
        #print(paste(n, "-", nodes_panel_update[n, "outp"], "to", unlist(nodes[n])))
        #count
        COUNTER <- c(COUNTER, rep(nodes_panel_update[n, "outp"], length(unlist(nodes[n]))))
        #update what's next
        next_nodes_to_visit <- c(next_nodes_to_visit, unlist(nodes[n]))
      }
    }
    nodes_to_visit <- next_nodes_to_visit
    nodes_panel <- nodes_panel_update
  }
  
  #upon pushing the button again, make sure that all flip-flops that are high get set back to low - nothing happened
  nodes_panel[which((nodes_panel$type == "%") & (nodes_panel$inp == "high")), "inp"] <- "low"
}

table(COUNTER)

prod(table(COUNTER))

## VISUALISATION
edges <- c()
for(i in 1:length(nodes)) {
  for(j in 1:length(nodes[[i]]))
    edges <- c(edges, names(nodes)[i], nodes[[i]][j]) 
}

g <- make_graph(edges, directed = TRUE)

plot(g, edge.arrow.size=0.1, vertex.color="orange", vertex.size=8,
     vertex.frame.color="gray", vertex.label.color="black",
     vertex.label.cex=0.7, vertex.label.dist=0.1, edge.curved=0.2)



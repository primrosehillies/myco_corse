degree <- table(c(edge$from, edge$to))
nodes <- data.frame(name = unique(c(edge$from, edge$to)),
                    group = NA,  # Placeholder for grouping
                    size = as.numeric(degree[as.character(unique(c(edge$from, edge$to)))] * 5))  # Scale font size




p <- simpleNetwork(edge, height="100px", width="100px",        
                   Source = 1,                 # column number of source
                   Target = 2,                 # column number of target
                   linkDistance = 5,          # distance between node. Increase this value to have more space between nodes
                   charge = -1000,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 30,               # size of the node names
                   fontFamily = "serif",       # font og node names
                   linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                   nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   zoom = T                    # Can you zoom on the figure?
)
p


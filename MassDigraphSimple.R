library(DiagrammeR)
grViz("
      
      digraph MB {
      
      rankdir=LR
      
      # add node statements
      node [shape = circle
      #fontname = helvetica
      #pendwidth = 2.0
      ]
      App;
      
      
      node [shape = square,
      style = filled]
      
      Plots [fillcolor = coral]; Outlet [fillcolor = steelblue]
      
      # add edge statements
      #edge [arrowhead = dimond]
      App -> Plots ; # [label = ' App.' ]; 
      Plots -> Outlet;
      Plots -> Other;
      
      
      # add a graph statement
      #graph [nodesep = 0.1]
      
      }
      ")
library(DiagrammeR)
grViz("

digraph MB {

rankdir=LR

# add node statements
node [shape = circle
      #fontname = helvetica
      #pendwidth = 2.0
      ]
      App; Runoff; Drains; Infil;
      
      
      node [shape = square,
            style = filled]

      Plot [fillcolor = coral]; Outlet [fillcolor = steelblue]; GW
      
      # add edge statements
      #edge [arrowhead = dimond]
      App -> Plot ; # [label = ' App.' ]; 
      Plot -> Runoff ; #[label = ' Run-off']; 
      Plot -> Drains ; # [label = ' Drainage' ];
      Plot -> Infil ;  #[label = ' Infil.' ]
      Plot -> Uptake;
      Plot -> Sorption;
      Plot -> Degradation;

      Runoff -> Outlet ; 
      Drains -> Outlet ; #[label = ' Channels' ]
      Infil -> Outlet ; #[label = ' Base flow.' ]
      Infil -> GW;
      
      
      # add a graph statement
      #graph [nodesep = 0.1]
      
}
")



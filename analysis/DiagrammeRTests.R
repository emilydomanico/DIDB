library(DiagrammeR)
library(tidyverse)

#test

# a_graph <- 
#   create_graph()%>%
#   add_node(label= "For.Error Test", tooltip = "Is the change real for at\n least one population?", node_aes(shape= "plaintext")) %>%
#   add_node(label = "Yes. real change") %>%
#   add_node(label = "No. No real change") %>%
#   add_edge(from= "For.Error Test", to = "Yes. real change")%>%
#   add_edge(from= "For.Error Test", to = "No. No real change")
# 
# render_graph(a_graph, layout = "tree")
# 
# 
# 
# node_list <- data.frame(id = c(1:5),
#                         type = c("test", "non, pro", "non, pro", "non, pro", "non, pro"), 
#                         label = c("Change Test", "Yes, Yes", "Yes, No", "No, Yes", "Yes, Yes"))
# edge_list <- data.frame(from= c(1, 1, 1, 1),
#                         to = c(2, 3, 4, 5))
# 
# graph_test <- create_graph()%>%
#   add_nodes_from_table(table= node_list, label_col = label)%>%
#   add_edges_from_table(table= edge_list, from_col = from, to_col = to)%>%
#   render_graph(layout = "tree")


grViz("digraph {

      graph [layout = dot,
       rankdir = LR]
       
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = plaintext]        
      tab1 [label = '@@1', shape= rectangle, tooltip= 'Test 1']
      tab2 [label = '@@2', fontcolor= 'red']
      tab3 [label = '@@3']
      tab4 [label = '@@4', shape= rectangle, tooltip= 'Test 2']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      tab11 [label = '@@11', fontcolor= 'red']
      tab12 [label = '@@12']
      tab13 [label = '@@13']
      tab14 [label = '@@14']
      tab15 [label = '@@15']
      tab16 [label = '@@16']
      tab17 [label = '@@17']
      tab18 [label = '@@18', shape= rectangle,tooltip= 'Test 3']
      tab19 [label = '@@19']
      tab20 [label = '@@20']
      tab21 [label = '@@21']
      tab22 [label = '@@18', shape= rectangle,tooltip= 'Test 3']
      tab23 [label = '@@19']
      tab24 [label = '@@20']
      tab25 [label = '@@21']
      tab26 [label = '@@18', shape= rectangle,tooltip= 'Test 3']
      tab27 [label = '@@19']
      tab28 [label = '@@20']
      tab29 [label = '@@21']
      tab30 [label = '@@18', shape= rectangle,tooltip= 'Test 3']
      tab31 [label = '@@19']
      tab32 [label = '@@20']
      tab33 [label = '@@21']
      tab34 [label = '@@18', shape= rectangle,tooltip= 'Test 3']
      tab35 [label = '@@19']
      tab36 [label = '@@20']
      tab37 [label = '@@21']
      tab38 [label = '@@18', shape= rectangle,tooltip= 'Test 3']
      tab39 [label = '@@19']
      tab40 [label = '@@20']
      tab41 [label = '@@21']
      tab42 [label = '@@18', shape= rectangle,tooltip= 'Test 3']
      tab43 [label = '@@19']
      tab44 [label = '@@20']
      tab45 [label = '@@21']
      tab46 [label = '@@18', shape= rectangle,tooltip= 'Test 3']
      tab47 [label = '@@19']
      tab48 [label = '@@20']
      tab49 [label = '@@21']
      tab50 [label = '@@18', shape= rectangle,tooltip= 'Test 3']
      tab51 [label = '@@19']
      tab52 [label = '@@20']
      tab53 [label = '@@21']
      tab54 [label = '@@18', shape= rectangle,tooltip= 'Test 3']
      tab55 [label = '@@19']
      tab56 [label = '@@20']
      tab57 [label = '@@21']
      tab58 [label = '@@18', shape= rectangle,tooltip= 'Test 3']
      tab59 [label = '@@19']
      tab60 [label = '@@20']
      tab61 [label = '@@21']
      tab62 [label = '@@18', shape= rectangle,tooltip= 'Test 3']
      tab63 [label = '@@19']
      tab64 [label = '@@20']
      tab65 [label = '@@21']
  

      # edge definitions with the node IDs
      tab1 -> tab2;
      tab1 -> tab3 -> tab4;
      tab4 -> tab5 -> tab62 -> tab63;
      tab62 -> tab64;
      tab62 -> tab65;
      tab4 -> tab6 -> tab58 -> tab59;
      tab58 -> tab60;
      tab58 -> tab61;
      tab4 -> tab7 -> tab54 -> tab55;
      tab54 -> tab56;
      tab54 -> tab57;
      tab4 -> tab8 -> tab50 -> tab51;
      tab50 -> tab52;
      tab50 -> tab53;
      tab4 -> tab9-> tab46 -> tab47;
      tab46 -> tab48;
      tab46 -> tab49;
      tab4 -> tab10 -> tab42 -> tab43;
      tab42 -> tab44;
      tab42-> tab45;
      tab4 -> tab11;
      tab4 -> tab12 -> tab38 -> tab39;
      tab38 -> tab40;
      tab38 -> tab41;
      tab4 -> tab13-> tab34 -> tab35;
      tab34 -> tab36;
      tab34 -> tab37;
      tab4 -> tab14-> tab30 -> tab31;
      tab30 -> tab32;
      tab30 -> tab33;
      tab4 -> tab15 -> tab26 -> tab27;
      tab26 -> tab28;
      tab26 -> tab29;
      tab4 -> tab16 -> tab22-> tab23;
      tab22 -> tab24;
      tab22 -> tab25;
      tab4 -> tab17 -> tab18 ->tab19;
      tab18 -> tab20;
      tab18 -> tab21
      }

      [1]: 'Is there real change for at least one population?'
      [2]: 'No'
      [3]: 'Yes'
      [4]: 'How are populations impacted?'
      [5]: 'Benefit for both'
      [6]: 'Only benefits \\nprotected population, \\nbenefits non-protected \\npopulation within threshold'
      [7]: 'Benefits protected population, \\nburdens non-protected population'
      [8]: 'Only benefits \\nprotected population, \\nburdens non-protected \\npopulation within threshold'
      [9]: 'Only benefits \\nnon-protected population, \\nbenefits protected \\npopulation within the threshold'
      [10]: 'Only benefits \\nnon-protected population, \\nburdens protected \\npopulation within threshold'
      [11]: 'Impact within \\nthreshold for both'
      [12]: 'Only burdens \\nnon-protected population, \\nbenefits protected \\npopulation within threshold'
      [13]: 'Only burdens \\nnon-protected population, \\nburdens protected \\npopulation within the threshold'
      [14]: 'Burdens protected \\npopulation, \\nbenefits non-protected \\npopulation'
      [15]: 'Only burdens \\nprotected population, \\nbenefits non-protected \\npopulation within threshold'
      [16]: 'Only burdens \\nprotected population, \\nburdens non-protected \\npopulation within threshold'
      [17]: 'Burden for both'
      [18]: 'Who is impacted more?'
      [19]: 'Protected more affected'
      [20]: 'Non-protected more affected'
      [21]: 'Disproportionality within threshold'
      ")



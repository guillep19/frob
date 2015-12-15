#include "frp.h"
#include "globals.h"

//FRP combinators
void f_lift() {
    //lift inm=id word=source word=function_loc
    WORD id = inm;
    WORD source = *ip++;
    WORD function_loc = *ip++;
    WORD source_pos = find_node(graph, source);
    if ((source_pos == -1)||(find_node(graph, id) != -1)) {
      return;
    }
    WORD dest_pos = create_node(graph, id, function_loc, 1);
    link_nodes(graph, source_pos, dest_pos, 0);
}
void f_lift2() {
  WORD id = inm;
  WORD s1 = *ip++;
  WORD s2 = *ip++;
  WORD function_loc = *ip++;
  WORD s1_pos = find_node(graph, s1);
  WORD s2_pos = find_node(graph, s2);
  if ((s1_pos == -1)||(s2_pos == -1)) {
    return;
  }
  if (find_node(graph, id) != -1) {
    return;
  }
  WORD dest_pos = create_node(graph, id, function_loc, 2);
  link_nodes(graph, s1_pos, dest_pos, 0);
  link_nodes(graph, s2_pos, dest_pos, 1);
}
void f_folds() {
  BYTE id = inm;
  BYTE source = *ip++;
  WORD acum = globals[*ip++];
  WORD function_loc = *ip++;
  WORD source_pos = find_node(graph, source);
  if ((source_pos == -1) || (find_node(graph, id) != -1)) {
    return;
  }
  WORD node_pos = create_fold_node(graph, id, function_loc, acum);
  link_nodes(graph, source_pos, node_pos, 0);
}
//IO
void f_read() {
  //read inm=id word=input
  WORD id = inm;
  WORD input = globals[*ip++];
  //add graph node
  WORD node_pos = create_node(graph, id, -1, 1);
  //link input to node
  link_input(graph, input, node_pos);
}
void f_write() {
  //write inm=id word=output
  WORD id = inm;
  WORD output = *ip++;
  WORD node_pos = find_node(graph, id);
  if (node_pos == -1) {
    return;
  }
  link_output(graph, output, id);
}

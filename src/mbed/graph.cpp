


#include "graph.h"

Graph create_graph() {
  //Reset inputs
  Graph graph;

  WORD iter;
  for (iter = 0; iter < 10; iter++) {
    graph.inputs[iter].fwd_count = 0;
  }
  //Reset nodes
  graph.count = 0;
  graph.ready_next = 0;
  graph.ready_end = 0;
  //Reset outputs
  for (iter = 0; iter < 10; iter++) {
    graph.outputs[iter].source = -1;
  }
  return graph;
}

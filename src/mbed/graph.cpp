

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

/*
 * Returns the index of the node id in the graph.
 * If the id does not exist, it returns -1.
 */
WORD find_node(Graph &graph, BYTE id) {
  WORD pos = 0;
  while ((pos < graph.count) && (graph.nodes[pos].id != id)) pos++;
  if (pos == graph.count) pos = -1;
  return pos; 
}

/*
  Creates a node in the graph passed by reference.
  The id of the node must not exist. (find_node(graph, id) == -1)
  Returns the index of the node in the graph.
*/
WORD create_node(Graph &graph, BYTE id, WORD function_loc, BYTE arg_count) {
  WORD position = graph.count++;
  graph.nodes[position].id = id; //define signal id
  graph.nodes[position].arg_count = arg_count;
  graph.nodes[position].function_loc = function_loc;
  graph.nodes[position].fwd_count = 0;
  return position;
}

/*
 * Creates a link between graph.nodes[src] and 
 * graph.nodes[dest].
 * src and dest are the indexes of the nodes
 * to lift.
 * Pre: src and dest are valid nodes.
 */
void link_nodes(Graph &graph, BYTE src, BYTE dest, BYTE arg_place) {
  BYTE fwd_count = graph.nodes[src].fwd_count;
  graph.nodes[src].fwd[fwd_count] = dest;
  graph.nodes[src].fwd_place[fwd_count] = arg_place;
  graph.nodes[src].fwd_count++;
}

/*
 * Links an input to a graph node.
 * Pre: The id of the node must exist. (find_node(graph, id) != -1)
 */
void link_input(Graph &graph, WORD input, BYTE id) {
  BYTE fwd_count = graph.inputs[input].fwd_count;
  graph.inputs[input].fwd[fwd_count] = id;
  graph.inputs[input].fwd_count++;
}

/*
 * Links a graph node to an output.
 * Pre: The id of the node must exist. (find_node(graph, id) != -1)
 */
void link_output(Graph &graph, WORD output, BYTE id) {
  graph.outputs[output].source = id;
}

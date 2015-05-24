
#ifndef GRAPH_H
#define GRAPH_H

#include "FrobDefinitions.h"

struct Node {
  BYTE id;
  WORD arg[2];
  BYTE arg_new[2]; //arg is new or used
  BYTE arg_count;
  WORD function_loc;
  WORD fwd[5]; //Nodes to fwd data
  BYTE fwd_place[5]; //where to fwd
  BYTE fwd_count;
  WORD value;
};

struct Input {
  WORD fwd[5];
  BYTE fwd_count;
};

struct Output {
  WORD source;
};

struct Graph {
  Input inputs[10];
  Output outputs[10];
  Node nodes[20];
  WORD count;
  BYTE ready_nodes[20];
  BYTE ready_next;
  BYTE ready_end;
};

Graph create_graph();

WORD find_node(Graph &graph, BYTE id);

WORD create_node(Graph &graph, BYTE id, WORD function_loc, BYTE arg_count);

void link_nodes(Graph &graph, BYTE src, BYTE dest, BYTE arg_place);

void link_input(Graph &graph, WORD input, BYTE id);

void link_output(Graph &graph, WORD output, BYTE id);

#endif /* GRAPH_H */

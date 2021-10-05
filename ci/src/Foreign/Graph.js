const graphlib = require('graphlib');
const Graph = graphlib.Graph;

exports.createImpl = function(nodes, edges) {
  const g = new Graph();
  nodes.forEach(node => {
    g.setNode(node.id, node.v);
  });
  edges.forEach(edge => {
    g.setEdge(edge.from, edge.to);
  });
  return g;
};

exports.topsortImpl = function(graph) {
  return graphlib.alg.topsort(graph);
};

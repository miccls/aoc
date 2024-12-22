import os

def read_file(path):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(script_dir, path)
    with open(file_path, "r") as f:
        text = f.read()
    return text

import heapq

def dijkstra(graph, start):
    """
    Perform Dijkstra's algorithm to find the shortest paths from a start node to all other nodes in the graph.

    :param graph: A dictionary where keys are node names and values are lists of tuples (neighbor, weight).
                  For example: {'A': [('B', 1), ('C', 4)], 'B': [('A', 1), ('C', 2), ('D', 5)], ...}
    :param start: The starting node.
    :return: A dictionary with the shortest distances to each node from the start node.
             A dictionary with the previous node in the shortest path for each node.
    """
    # Priority queue to store the next most promising node
    priority_queue = []
    heapq.heappush(priority_queue, (0, start))  # (distance, node)

    # Dictionary to store the shortest known distance to each node
    distances = {node: float('inf') for node in graph}
    distances[start] = 0

    # Dictionary to reconstruct the shortest path
    previous_nodes = {node: None for node in graph}

    while priority_queue:
        current_distance, current_node = heapq.heappop(priority_queue)

        # If the current distance is greater than the recorded distance, skip
        if current_distance > distances[current_node]:
            continue

        for neighbor, weight in graph[current_node]:
            distance = current_distance + weight

            # Only consider this path if it's better
            if distance < distances[neighbor]:
                distances[neighbor] = distance
                previous_nodes[neighbor] = current_node
                heapq.heappush(priority_queue, (distance, neighbor))

    return distances, previous_nodes

# Use Dijkstras to find path

# 1. Begin by computing the grid.
text = read_file("input.txt")
dirs = [(0,1), (0,-1),(1,0),(-1,0)]
n = 2881
mem_coords = [(int(s.split(",")[0]), int(int(s.split(",")[1]))) for s in text.split("\n")[:n]]
print(text.split("\n")[n])
#print(mem_coords)
inside = lambda x, y: (x < 71 and x>=0) and (y < 71 and y>=0)
graph = {(i,j):[((i+dx,j+dy), 1 if (i+dx,j+dy) not in mem_coords else float("inf")) for dx, dy in dirs if inside(i+dx, j+dy)] for i in range(71) for j in range(71)}
start = (0,0)
distances, previous_nodes = dijkstra(graph, start)
print(n, ":", distances[(70,70)])

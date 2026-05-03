#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#include <string.h>

#define MAX_GRID 100
#define INF 1e9

// Structure to represent a point on the 2D grid 
typedef struct {
    int x, y;
} Point;

// Structure to represent a node in the A* algorithm
typedef struct Node {
    Point pos;
    double g_score;  // Cost from start to current node 
    double h_score;  // Heuristic (estimated cost to goal)
    double f_score;  // f = g + h
    struct Node* parent;
} Node;

// --- PRIORITY QUEUE (MIN-HEAP) IMPLEMENTATION ---

typedef struct {
    Node** nodes;
    int size;
    int capacity;
} MinHeap;

MinHeap* create_heap(int capacity) {
    MinHeap* heap = (MinHeap*)malloc(sizof(MinHeap));
    heap->nodes = (Node**)malloc(sizeof(Node*) * capacity);
    heap->size = 0;
    heap->capacity = capacity;
    return heap;
}

void swap_nodes(Node** a, Node** b) {
    Node* temp = *a;
    *a = *b;
    *b = temp;
}

void heapify_up(MinHeap* heap, int idx) {
    while(idx > 0) {
      int parent = (idx - 1) / 2;
      if(heap->nodes[idx]->f_score < heap->nodes[parent]->f_score) {
        swap_nodes(&heap->nodes[idx], &heap->nodes[parent]);
        idx = parent;
      } else {
        break;
      }
    }
}

void heapify_down(MinHeap* heap, int idx) {
    int smallest = idx;
    int left = 2 * idx + 1;
    int right = 2 * idx + 2;

    if(left < heap->size && heap->nodes[left]->f_score < heap->nodes[smallest]->f_score)
      smallest = left;
    if(right < heap->size && heap->nodes[right]->f_score < heap->nodes[smallest]->f_score)
      smallest = right;

    if(smallest != idx) {
      swap_nodes(&heap->nodes[idx], &heap->nodes[smallest]);
      heapify_down(heap, smallest);
    }
}

void push_heap(MinHeap* heap, Node* node) {
    if(heap->size == heap->capacity) return;
    heap->nodes[heap->size] = node;
    heapify_up(heap, heap->size);
    heap->size++;
}

Node* pop_heap(MinHeap* heap) {
    if(heap->size == 0) return NULL;
    Node* root = heap->nodes[0];
    heap->nodes[0] = heap->nodes[heap->size - 1];
    heap->size--;
    heapify_down(heap, 0);
    return root;
}

bool is_heap_empty(MinHeap* heap) {
    return heap->size == 0;
}

// ---- UTILITY FUNCTIONS ----

double calculate_heuristic(Point a, Point b) {
    // Euclidean Distance
    return sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2));
    // Manhattan Distance (Alternative for 4-way movement):
    // return abs(a.x - b.x) + abs(a.y - b.y);
}

bool is_valid(int x, int y, int rows, int cols, int grid[MAX_GRID][MAX_GRID]) {
    return (x >= 0 && x < rows && y >= 0 && y < cols && grid[x][y] == 0);
}

void print_path(Node* goal_mode) {
    if(goal_mode == NULL) {
    printf("No path found.\n");
    return;
    }
    Node* curr = goal_mode;
    printf("Path found (Goal to Start):\n");
    while(curr != NULL) {
      printf("(%d, %d)", curr->pos.x, curr->pos.y);
      if(curr->parent != NULL) printf("<-");
      curr = curr->parent;
    }
    printf("\n");
}

// --- CORE A* ALGORITHM ----

void a_star(int grid[MAX_GRID][MAX_GRID], int rows, int cols, Point start, Point goal) {
    // Closed set: 2D array to keep track of visited nodes to their scores
    // In C, we often use a matrix to store node pointers for O(1) access
    
    Node* all_nodes[MAX_GRID][MAX_GRID];
    bool closed_set[MAX_GRID][MAX_GRID];

    for(int i = 0; i < rows; i++) {
      for(int j = 0; j < cols; j++) {
        all_nodes[i][j] = NULL; 
        closed_set[i][j] = false;
      }
    }

    MinHeap* open_set = create_heap(rows * cols);

    // Initialize start node 
    Node* start_node = (Node*)malloc(sizeof(Node));
    start_node->pos = start;
    start_node->g_score = 0;
    start_node->h_score = calculate_heuristic(start, goal);
    start_node->f_score = start_node->g_score + start_node->h_score;
    start_node->parent = NULL;

    all_nodes[start.x][start.y] = start_node;
    push_heap(open_set, start_node);

    // Direction vectors (8-way movement: Up, Down, Left, Right, and Diagonals)
    int dx[] = {-1, 1, 0, 0, -1, -1, 1, 1};
    int dy[] = {0, 0, -1, 1, -1, 1, -1, 1};
    double costs[] = {1.0, 1.0, 1.0, 1.0, 1.414, 1.414, 1.414, 1.414};  // 1.414 is approx sqrt(2)

    bool found_gaol = false;
    Node* final_node = NULL;

    while(!is_heap_empty(open_set)) {
        Node* current = pop_heap(open_set);

        // If goal is reached
        if(current->pos.x == goal.x && current->pos.y == goal.y) {
          found_goal = true;
          final_node = current;
          break;
        }

        closed_set[current->pos.x][current->pos.y] = true;

        // Explore 8 neighbors
        for(int i = 0; i < 8; i++) {
            int nx = current->pos.x + dx[i];
            int ny = current->pos.y + dy[i];

            if(!is_valid(nx, ny, rows, cols, grid) || closed_grid[nx][ny]) {
               continue;
            } 

            double tentative_g = current->g_score + costs[i];

            // If node hasn't been discovered, or we found a better path to it
            if(all_nodes[nx][ny] == NULL || tentative_g < all_nodes[nx][ny]->g_score) {

              
            }
        }
    }
    
}

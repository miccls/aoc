#include <algorithm>  // Required for max/min
#include <chrono>
#include <fstream>
#include <iostream>
#include <numeric>  // Required for std::accumulate
#include <string>
#include <vector>
#include <format>

// CUDA includes
#include <cuda_runtime.h>
#include <device_launch_parameters.h>


// We must define block size at compile time for 2D shared memory arrays
#define TILE_SIZE 32
constexpr bool kPart2 = false;

// Helper macro to check boundaries: TODO: Make into device function.
#define IN_BOUNDS(r, c, rows, cols) \
  ((r) >= 0 && (r) < (rows) && (c) >= 0 && (c) < (cols))

std::vector<std::string> readLines(const std::string& filename) {
  std::vector<std::string> lines;
  std::ifstream file(filename);

  if (file.is_open()) {
    std::string line;
    while (std::getline(file, line)) {
      lines.push_back(line);
    }
    file.close();
  } else {
    std::cerr << "Unable to open file\n";
  }
  return lines;
}

std::vector<uint8_t> processData(const std::vector<std::string>& input_grid) {
  if (input_grid.empty()) return {};

  const auto rows = input_grid.size();
  const auto cols = input_grid.at(0).size();
  const char paper_roll = '@';

  std::vector<uint8_t> has_paper_roll(rows * cols);
  for (size_t row = 0; row < rows; ++row) {
    for (size_t col = 0; col < cols; ++col) {
      // Standard row-major flattening: row * width + col
      has_paper_roll[row * cols + col] =
          (input_grid[row][col] == paper_roll) ? 1 : 0;
    }
  }
  return has_paper_roll;
}

__device__ void loadTile(uint8_t tile[TILE_SIZE + 2][TILE_SIZE + 2],
                         const uint8_t* __restrict__ data, int rows, int cols) {
  int tid = threadIdx.y * blockDim.x + threadIdx.x;
  int block_pixels = blockDim.x * blockDim.y;
  int smem_w = TILE_SIZE + 2;
  int count = smem_w * smem_w;

  for (int i = tid; i < count; i += block_pixels) {
    int r = i / smem_w;
    int c = i % smem_w;
    int gr = (blockIdx.y * blockDim.y) + r - 1;
    int gc = (blockIdx.x * blockDim.x) + c - 1;

    if (IN_BOUNDS(gr, gc, rows, cols)) {
      tile[r][c] = data[gr * cols + gc];
    } else {
      tile[r][c] = 0;
    }
  }
  __syncthreads();  // Wait for load to finish
}

__device__ uint8_t checkRoll(uint8_t tile[TILE_SIZE + 2][TILE_SIZE + 2],
                             int rows, int cols) {
  uint8_t can_remove = 0;
  // Coordinates relative to Shared Memory
  int local_r = threadIdx.y + 1;
  int local_c = threadIdx.x + 1;
  int global_r = blockIdx.y * blockDim.y + threadIdx.y;
  int global_c = blockIdx.x * blockDim.x + threadIdx.x;

  // Check bounds for the actual computation
  if (global_r < rows && global_c < cols) {
    int center = tile[local_r][local_c];
    if (center == 1) {
      int neighbors =
          tile[local_r - 1][local_c - 1] + tile[local_r - 1][local_c] +
          tile[local_r - 1][local_c + 1] + tile[local_r][local_c - 1] +
          tile[local_r][local_c + 1] + tile[local_r + 1][local_c - 1] +
          tile[local_r + 1][local_c] + tile[local_r + 1][local_c + 1];
      if (neighbors <= 3) {
        can_remove = 1;
      }
    }
  }
  return can_remove;
}

__global__ void filterAndReduce(uint8_t* __restrict__ input,
                                int* global_count,  // Single output variable
                                int rows, int cols) {
  // Size: (32+2)x(32+2) = 1156 ints.
  // We reuse this memory: First for Input Halo, then for Reduction.
  // We use int (4 bytes) to prevent overflow during summation.
  __shared__ uint8_t smem[TILE_SIZE + 2][TILE_SIZE + 2];

  loadTile(smem, input, rows, cols);
  const auto can_be_removed = checkRoll(smem, rows, cols);
  __syncthreads();

  if (kPart2) {
    int gr = (blockIdx.y * blockDim.y) + threadIdx.y;
    int gc = (blockIdx.x * blockDim.x) + threadIdx.x;
    if (can_be_removed == 1) {
      input[gr * cols + gc] = 0;  // Remove if possible
    }
  }

  // Reuse smem as a linear array for reduction.
  // We map the 2D smem to a 1D pointer for easier math.
  uint8_t* smem_1d = (uint8_t*)&smem[0][0];

  // Write my result to shared memory
  int tid = threadIdx.y * blockDim.x + threadIdx.x;
  smem_1d[tid] = can_be_removed;

  __syncthreads();

  // Reduce each block.
  int paper_roll_grid = blockDim.x * blockDim.y;
  for (unsigned int s = paper_roll_grid / 2; s > 0; s >>= 1) {
    if (tid < s) {
      smem_1d[tid] += smem_1d[tid + s];
    }
    __syncthreads();
  }

  __syncthreads();
  // We could do a grid-wide reduce, but I think this will do.
  if (tid == 0) {
    atomicAdd(global_count, (int)smem_1d[0]);
  }
}

int main() {
  const auto lines = readLines("input.txt");
  if (lines.empty()) {
    std::cerr << "Input empty or file not found.\n";
    return -1;
  }

  auto start_time = std::chrono::high_resolution_clock::now();

  const int rows = static_cast<int>(lines.size());
  const int cols = static_cast<int>(lines.at(0).size());

  const auto h_input_flat = processData(lines);

  // Allocate Device Memory
  uint8_t* dev_grid;
  int* dev_results;
  size_t bytes = h_input_flat.size() * sizeof(uint8_t);

  cudaMalloc(&dev_grid, bytes);
  cudaMalloc(&dev_results, sizeof(int));
  cudaMemset(dev_results, 0, sizeof(int));

  auto start_h2d = std::chrono::high_resolution_clock::now();
  cudaMemcpy(dev_grid, h_input_flat.data(), bytes, cudaMemcpyHostToDevice);
  auto end_h2d = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double, std::milli> time_h2d = end_h2d - start_h2d;

  dim3 threads(TILE_SIZE, TILE_SIZE);
  dim3 blocks((cols + threads.x - 1) / threads.x,
              (rows + threads.y - 1) / threads.y);

  int valid_rolls = 0;

  if (!kPart2) {
    // Run the kernel once to time prime it for timing
    filterAndReduce<<<blocks, threads>>>(dev_grid, dev_results, rows, cols);
    cudaMemset(dev_results, 0, sizeof(int));
  }

  auto start_kernel = std::chrono::high_resolution_clock::now();
  if (kPart2) {
    while (true) {
      int prev_rolls = valid_rolls;
      filterAndReduce<<<blocks, threads>>>(dev_grid, dev_results, rows, cols);
      cudaMemcpy(&valid_rolls, dev_results, sizeof(int),
                 cudaMemcpyDeviceToHost);
      if (valid_rolls == prev_rolls) {
        break;  // 0 rolls were added.
      }
    }

  } else {
    filterAndReduce<<<blocks, threads>>>(dev_grid, dev_results, rows, cols);
  }
  cudaDeviceSynchronize();

  auto end_kernel = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double, std::milli> time_kernel =
      end_kernel - start_kernel;

  // 5. Copy Results Back

  auto start_d2h = std::chrono::high_resolution_clock::now();

  cudaMemcpy(&valid_rolls, dev_results, sizeof(int), cudaMemcpyDeviceToHost);

  auto end_d2h = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double, std::milli> time_d2h = end_d2h - start_d2h;

  std::cout << "H2D Copy: " << time_h2d.count() << " ms\n";
  std::cout << "Kernel:   " << time_kernel.count() << " ms\n";
  std::cout << "D2H Copy: " << time_d2h.count() << " ms\n";

  std::cout << std::format("Answer to part {}:  ", kPart2 ? "2" : "1") << valid_rolls << std::endl;

  cudaFree(dev_grid);
  cudaFree(dev_results);
  return 0;
}
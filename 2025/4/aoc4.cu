#include <algorithm>
#include <chrono>
#include <format>
#include <fstream>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>

// CUDA includes
#include <cuda_runtime.h>
#include <device_launch_parameters.h>

constexpr size_t kTileSize = 32;
constexpr bool kPart2 = false;

__device__ bool inBounds(int row, int col, int rows, int cols) {
    return ((row) >= 0 && (row) < (rows) && (col) >= 0 && (col) < (cols));
}

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
    constexpr char paper_roll = '@';

    std::vector<uint8_t> has_paper_roll(rows * cols);
    for (size_t row = 0; row < rows; ++row) {
        for (size_t col = 0; col < cols; ++col) {
            has_paper_roll[row * cols + col] =
                (input_grid[row][col] == paper_roll) ? 1 : 0;
        }
    }
    return has_paper_roll;
}

__device__ void loadTile(uint8_t tile[kTileSize + 2][kTileSize + 2],
                         const uint8_t* __restrict__ data, int rows, int cols) {
    int tid = threadIdx.y * blockDim.x + threadIdx.x;
    int block_pixels = blockDim.x * blockDim.y;
    int smem_w = kTileSize + 2;
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
    __syncthreads();
}

__device__ uint8_t checkRoll(uint8_t tile[kTileSize + 2][kTileSize + 2],
                             int rows, int cols) {
    uint8_t can_remove = 0;

    // Block coordinates for shared.
    int local_r = threadIdx.y + 1;
    int local_c = threadIdx.x + 1;

    int global_r = blockIdx.y * blockDim.y + threadIdx.y;
    int global_c = blockIdx.x * blockDim.x + threadIdx.x;

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

__global__ void filterAndReduce(uint8_t* __restrict__ input, int* global_count,
                                int rows, int cols) {
    // Halo of one on each size
    __shared__ uint8_t
        smem[kTileSize + 2]
            [kTileSize +
             2];  // Using shared, similar to stencile approach in Ansorge's
                  // book Recent caching techniques in later GPU's probably make
                  // this less impactful...

    loadTile(smem, input, rows, cols);
    const auto can_be_removed = checkRoll(smem, rows, cols);
    __syncthreads();

    if (kPart2) {
        int gr = (blockIdx.y * blockDim.y) + threadIdx.y;
        int gc = (blockIdx.x * blockDim.x) + threadIdx.x;
        if (can_be_removed == 1) {
            input[gr * cols + gc] = 0;
        }
    }

    uint8_t* smem_1d = (uint8_t*)&smem[0][0];

    int tid = threadIdx.y * blockDim.x + threadIdx.x;
    smem_1d[tid] = can_be_removed;

    __syncthreads();

    // Block-wide reduce
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

    dim3 threads(kTileSize, kTileSize);
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
            filterAndReduce<<<blocks, threads>>>(dev_grid, dev_results, rows,
                                                 cols);
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

    auto start_d2h = std::chrono::high_resolution_clock::now();

    cudaMemcpy(&valid_rolls, dev_results, sizeof(int), cudaMemcpyDeviceToHost);

    auto end_d2h = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> time_d2h = end_d2h - start_d2h;

    std::cout << "H2D Copy: " << time_h2d.count() << " ms\n";
    std::cout << "Kernel:   " << time_kernel.count() << " ms\n";
    std::cout << "D2H Copy: " << time_d2h.count() << " ms\n";

    std::cout << std::format("Answer to part {}:  ", kPart2 ? "2" : "1")
              << valid_rolls << std::endl;

    cudaFree(dev_grid);
    cudaFree(dev_results);
    return 0;
}
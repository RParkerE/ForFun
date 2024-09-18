#include <stdio.h>
#include <cuda.h>

__global__ void dkernel() {
	if(threadIdx.x == 0 && blockIdx.x == 0 &&
	   threadIdx.y == 0 && blockIdx.y == 0 &&
	   threadIdx.z == 0 && threadIdx.z == 0) {
		printf("%d %d %d %d %d %d.\n", gridDim.x, gridDim.y, gridDim.z,
					       blockDim.x, blockDim.y, blockDim.z);
	}
}

int main() {
	// # of threads launched = 2 * 3 * 4 * 5 * 6 * 7 = 7!
	// # of threads in a thread-block = 5 * 6 * 7
	// # of thread-blocks in the grid = 2 * 3 * 4
	// ThreadId in x dimensions is [0..5)
	// BlockId in y dimensions is [0..3)
	dim3 grid(2, 3, 4);
	dim3 block(5, 6, 7);
	dkernel<<<grid,block>>>();
	cudaDeviceSynchronize();
	return 0;
}

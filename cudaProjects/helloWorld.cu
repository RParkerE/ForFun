#include <stdio.h>
#include <cuda.h>

// Kernel
__global__ void dkernel() {
	printf("Hello World!\n");
}

int main() {
	// Kernel Launch
	// Kernels (by default) are executed one after another
	// CPU Launches and moves ahead
	dkernel<<<1,24>>>();
	dkernel<<<1,1>>>();
	// GPU and CPU run asynchronously this forces CPU to waits here until kernels return
	cudaDeviceSynchronize();
	printf("On CPU [After GPU]\n");
	return 0;
}

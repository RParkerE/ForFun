#include <stdio.h>
#include <cuda.h>

// This is okay
#define msg "Hello World!\n"

// This is an error as GPU and CPU memory are seperate
const char *badMsg = "Hello World!\n";

__global__ void dkernel() {
	printf(msg);
}

int main() {
	dkernel<<<1,32>>>();
	cudaDeviceSynchronize();
	return 0;
}

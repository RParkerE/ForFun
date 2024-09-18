#include <stdio.h>
#include <cuda.h>

__global__ void dkernel(char *arr, int arrlen) {
	unsigned id = threadIdx.x;
	if (id < arrlen) {
		++arr[id];
	}
}

int main() {
	char cpuarr[] = "Gdkkn\x1Vnqkc-", 
		*gpuarr;
	cudaMalloc(&gpuarr, sizeof(char) * (1 + strlen(cpuarr)));
	cudaMemcpy(gpuarr, cpuarr, sizeof(char) * (1 + strlen(cpuarr)), cudaMemcpyHostToDevice);
	dkernel<<<1,32>>>(gpuarr, strlen(cpuarr));
	cudaMemcpy(cpuarr, gpuarr, sizeof(char) * (1 + strlen(cpuarr)), cudaMemcpyDeviceToHost);
	printf(cpuarr);
	return 0;
}

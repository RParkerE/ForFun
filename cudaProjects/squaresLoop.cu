#include <stdio.h>
#include <cuda.h>

// int main() {
//	int i;
//	for(i = 0; i < 100; ++i) {
//		printf("%d\n", i * i);
//	}
//	return 0;
//}

__global__ void squares() {
	printf("%d\n", threadIdx.x * threadIdx.x);
}

int main() {
	squares<<<1,100>>>();
	cudaDeviceSynchronize();
	return 0;
}

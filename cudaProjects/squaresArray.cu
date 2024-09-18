#include <stdio.h>
#include <cuda.h>

// int main() {
//	int a[100], i;
//	for(i = 0; i < 100; ++i) {
//		a[i] = i * i;
//	}
//	return 0;
// }

__global__ void squares(int *a) {
	a[threadIdx.x] = threadIdx.x * threadIdx.x;
}

int main() {
	int a[100], *da;
	cudaMalloc(&da, 100 * sizeof(int));
	squares<<<1,100>>>(da);
	cudaMemcpy(a, da, 100 * sizeof(int), cudaMemcpyDeviceToHost);
	for(int i = 0; i < 100; ++i)
		printf("%d\n", a[i]);
	return 0;
}

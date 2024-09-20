#include <stdio.h>
#include <cuda.h>

// When different warp-threads execute different instructions, threads are said to diverge
// Hardware executes threads satisfying same condition together, ensuring other threads execute a no-op
// This adds sequentially to the execution
// This problem is termed as thread-divergence

// 0	1	2	3	4	5
// s0	s0	s0	s0	s0	s0
// |	s1	|	s1	|	s1
// s2	|	s2	|	s2	|
// s3	s3	s3	s3	s3	s3
// Where | = no-op


__global__ void dkernel(unsigned *vector, unsigned vectorsize) {
	//s0
	unsigned id = blockIdx.x * blockDim.x + threadIdx.x;
	//s1
	if(id % 2) vector[id] = id;
	//s2
	else vector[id] = vectorsize * vectorsize;
	//s3
	vector[id]++;
}

int main() {
	unsigned vectorsize = 32;
	unsigned *vector, *hvector;

	cudaMalloc(&vector, vectorsize * sizeof(unsigned));
	hvector = (unsigned *)malloc(vectorsize * sizeof(unsigned));

	dkernel<<<1,32>>>(vector, vectorsize);

	cudaMemcpy(hvector, vector, vectorsize * sizeof(unsigned), cudaMemcpyDeviceToHost);

	for(unsigned i = 0; i < vectorsize; ++i) {
		printf("hvector[%d] = %u\n", i, hvector[i]);
	}
	return 0;
}

#include <stdio.h>
#include <cuda.h>

//void squarecpu(unsigned *matrix, unsigned *result, unsigned matrixsize) {
//	for(unsigned ii = 0; ii < matrixsize; ++ii) {
//		for(unsigned jj = 0; jj < matrixsize; ++jj) {
//			for(unsigned kk = 0; kk < matrixsize; ++kk) {
//				result[ii * matrixsize + jj] += matrix[ii * matrixsize + kk] * matrix[kk * matrixsize + jj];
//			}
//		}
//	}
//}

__global__ void square(unsigned *matrix, unsigned *result, unsigned matrixsize) {
	unsigned id = blockIdx.x * blockDim.x + threadIdx.x;
	unsigned ii = id / matrixsize;
	unsigned jj = id % matrixsize;
	for(unsigned kk = 0; kk < matrixsize; ++kk) {
		result[ii * matrixsize + jj] += matrix[ii * matrixsize + kk] * matrix[kk * matrixsize + jj];
	}
}

#define N 64

int main() {
	unsigned *matrix, *hmatrix, *result, *hresult;
	cudaMalloc(&matrix, N * N * sizeof(unsigned));
	cudaMalloc(&result, N * N * sizeof(unsigned));
	hmatrix = (unsigned *)malloc(N * N * sizeof(unsigned));
	hresult = (unsigned *)malloc(N * N * sizeof(unsigned));

	for (unsigned i = 0; i < N * N; ++i) {
		hmatrix[i] = i % 10;
	}
	
	cudaMemcpy(matrix, hmatrix, N * N * sizeof(unsigned), cudaMemcpyHostToDevice);
	square<<<N,N>>>(matrix, result, N);
	cudaMemcpy(hresult, result, N * N * sizeof(unsigned), cudaMemcpyDeviceToHost);

	for (unsigned i = 0; i < N; ++i) {
		for (unsigned j = 0; j < N; ++j) {
			printf("%u ", hresult[i * N + j]);
		}
		printf("\n");
	}
	return 0;
}

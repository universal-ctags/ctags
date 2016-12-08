// C with some CUDA extensions.

__shared__ float2 array1[];
__shared__ __device__ dim3 array2[];
__device__ volatile int i1 = 1;
__constant__ int i2 = 2;

__device__ void dev1(float * a,const float * __restrict__ b)
{
}

__global__ void kernel1(float * a,float * __restrict__ b)
{
	
}

int main()
{
	kernel1<<<1024 * 1024 / 256,256>>>(NULL,NULL);
}


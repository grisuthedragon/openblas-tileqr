all: OpenBLAS libopenblas_serial.a libopenblas_openmp.a libopenblas_openmp_simple.a tileqr_serial tileqr_openmp tileqr_openmp_single

OpenBLAS: 
	if [ ! -d OpenBLAS ]; then git clone https://github.com/xianyi/OpenBLAS.git; else cd OpenBLAS; git pull; fi 

libopenblas_serial.a: OpenBLAS
	cd OpenBLAS ;\
	make clean; \
	make USE_OPENMP=0 USE_THREAD=0 NO_CBLAS=1 NO_LAPACKE=1 ; \
	cp libopenblas.a ../libopenblas_serial.a; \
	touch ../libopenblas_serial.a; 

libopenblas_openmp.a: OpenBLAS
	cd OpenBLAS ;\
	make clean; \
	make USE_OPENMP=1 USE_THREAD=1 NO_CBLAS=1 NO_LAPACKE=1 ; \
	cp libopenblas.a ../libopenblas_openmp.a; \
	touch ../libopenblas_openmp.a; 

libopenblas_openmp_simple.a: OpenBLAS
	cd OpenBLAS ;\
	make clean; \
	make USE_OPENMP=1 USE_THREAD=1 USE_SIMPLE_THREADED_LEVEL3=1 NO_CBLAS=1 NO_LAPACKE=1 ; \
	cp libopenblas.a ../libopenblas_openmp_simple.a; \
	touch ../libopenblas_openmp_simple.a; 


tileqr_serial: libopenblas_serial.a
	gfortran -o tileqr_serial -fopenmp tileqr.f90 libopenblas_serial.a -lgomp 

tileqr_openmp: libopenblas_openmp.a
	gfortran -o tileqr_openmp -fopenmp tileqr.f90 libopenblas_openmp.a -lgomp 

tileqr_openmp_simple: libopenblas_openmp_simple.a
	gfortran -o tileqr_openmp_simple -fopenmp tileqr.f90 libopenblas_openmp_simple.a -lgomp 

run: 
	./tileqr_serial 5120 5120 256 32
	./tileqr_openmp 5120 5120 256 32
	./tileqr_openmp_simple 5120 5120 256 32

clean: 
	rm -rf OpenBLAS 
	rm -f tileqr_serial tileqr_openmp tileqr_openmp_single
	rm -f *.a 

.PHONY: clean 

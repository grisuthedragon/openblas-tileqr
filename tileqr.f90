program main
  use omp_lib
  implicit none

  type block
     real(kind(1.d0)), allocatable :: b(:,:)
  end type block
  
  real(kind(1.d0)), allocatable :: a(:,:), t(:,:), acheck(:,:), tcheck(:,:), work(:,:), asic(:,:) 
  type(block), allocatable      :: a_b(:,:), t_b(:,:), work_b(:)
  integer                       :: q, p, b, ib, m, n, info, i, j, len, nargs
  real(kind(1.d0))              :: maxdiff1, maxdiff2, time, gflops, nrma, dnrm2

  integer(kind=8)               :: ts, te, tr
  character(LEN=20)             :: str

  nargs = command_argument_count()
  if(nargs .lt. 4) then
     write(*,'("m, n, b, ib")')
     stop
  end if

  call get_command_argument(1,value=str,length=len)
  read(str(1:len),*)m

  call get_command_argument(2,value=str,length=len)
  read(str(1:len),*)n

  call get_command_argument(3,value=str,length=len)
  read(str(1:len),*)b

  call get_command_argument(4,value=str,length=len)
  read(str(1:len),*)ib
  
  q  = (n-1)/b+1
  p  = (m-1)/b+1

  if (m .gt. n) then
     gflops = 2*real(n)*real(n)*(real(m) - real(n)/3.d0)/1d9
  else
     gflops = 2*real(m)*real(m)*(real(n) - real(m)/3.d0)/1d9
  end if
  
  allocate(a(m,n), t(p*ib,n))
  allocate(acheck(m,n), tcheck(ib,n), asic(m,n))
  
  call random_number(a)
  acheck=a
  asic = a 
  ! call columnwise_to_block(a, a_b, b)
  
  nrma = dnrm2(m*n, a, 1)
  
  allocate(work(b,n))

  call system_clock(ts)
  call dgeqrt(m, n, ib, &
       acheck(1,1), m, &
       t(1,1), ib, &
       work(1,1), info)
  call system_clock(te,tr)
  time = real(te-ts)/real(tr)
  write(*,'("LAPACK   -- Time: ",f8.3,"    Gflops/s: ",f8.3)')time, gflops/time

  deallocate(work)
  allocate(work(4*ib*b,omp_get_max_threads()))
  
  call system_clock(ts)
  call tileqr(a, t, b, ib, p, q, work)
  call system_clock(te,tr)
  time = real(te-ts)/real(tr)
  write(*,'("Tiled    -- Time: ",f8.3,"    Gflops/s: ",f8.3)')time, gflops/time
  

  maxdiff1 = 0.d0
  do i=1, m
     do j=1, n
        if(i.le.j) then
           maxdiff1=max(maxdiff1,abs(a(i,j))-abs(acheck(i,j)))
        end if
     end do
  end do


  write(*,'(" ")')
  write(*,'("Max diff between R from tiled   and LAPACK is:",e8.1)')maxdiff1/nrma

  
  stop
  
contains

  subroutine tileqr(a, t, b, ib, p, q, work) 
    use omp_lib
    implicit none

    
    
    real(kind(1.d0)) :: a(:,:), t(:,:), work(:,:)
    integer          :: b, ib, p, q
    
    integer          :: k, i, j, kk, ii, jj, lda, ldt, info, m, n
    integer          :: im, in, ik, iib

    m   = size(a,1)
    n   = size(a,2)
    lda = size(a,1)
    ldt = size(t,1)
    
    !$omp parallel 
    !$omp master
    do k =1, min(q,p)
       kk  = (k-1)*b+1
       im  = min(b,m-kk+1)
       ik  = min(b,n-kk+1)
       iib = min(ib,min(im,ik))
       !$omp task depend(inout:a(kk,kk),t((k-1)*ib+1,kk),a(kk+1,kk))&
       !$omp& firstprivate(im,ik,iib,kk,k) 
       call dgeqrt(im, ik, iib, &
            a(kk,kk), lda, &
            t((k-1)*ib+1,kk), ldt, &
            work(1,omp_get_thread_num()+1), &
            info)
       !$omp end task

       do j = k+1, q
          jj = (j-1)*b+1
          in = min(b,n-jj+1)
          !$omp task depend(in:a(kk+1,kk),t((k-1)*ib+1,kk)) depend(inout:a(kk,jj)) &
          !$omp& firstprivate(im,in,ik,iib,kk,k,jj) 
          call dgemqrt('l', 't', im, in, ik, iib, &
               a(kk,kk), lda, &
               t((k-1)*ib+1,kk), ldt, &
               a(kk,jj), lda, &
               work(1,omp_get_thread_num()+1), &
               info)
          !$omp end task

       end do

       do i = k+1, p
          ii = (i-1)*b+1
          im = min(b,m-ii+1)
          iib = min(ib,ik)
          !$omp task depend(inout:a(kk,kk),a(ii,kk),t((i-1)*ib+1,kk))&
          !$omp& firstprivate(im,ik,iib,kk,k,ii,i) 
          call dtpqrt(im, ik, 0, iib, &
               a(kk,kk), lda, &
               a(ii,kk), lda, &
               t((i-1)*ib+1,kk), ldt, &
               work(1,omp_get_thread_num()+1), &
               info)
               
          !$omp end task
       
          do j = k+1, q
             jj = (j-1)*b+1
             in = min(b,n-jj+1)
             !$omp task depend(in:a(ii,kk),t((i-1)*ib+1,kk)) depend(inout:a(kk,jj),a(ii,jj)) & 
             !$omp& firstprivate(im,in,ik,iib,kk,k,ii,jj,i) 
             call dtpmqrt('l', 't', im, in, ik, 0, iib, &
               a(ii,kk), lda, &
               t((i-1)*ib+1,kk), ldt, &
               a(kk,jj), lda, &
               a(ii,jj), lda, &
               work(1,omp_get_thread_num()+1), &
               info)
             !$omp end task
          end do
       end do
    end do
    !$omp end master
    !$omp end parallel
    
    
    return
  end subroutine tileqr


end program main
    

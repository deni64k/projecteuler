! Largest prime factor
! Problem 3
! The prime factors of 13195 are 5, 7, 13 and 29.
!
! What is the largest prime factor of the number 600851475143?

! Answer: 6857

program ex003
   implicit none

   integer(kind=8), parameter :: num = 600851475143_8
   integer, parameter :: sieve_size = floor(sqrt(real(num, 8)))
   integer :: res = 0

   print *, 'Sieve size:', sieve_size

   call each_prime(sieve_size, assign_prime_if_factor)

   print *, 'Answer:', res
contains
   subroutine each_prime(sieve_size, callback)
      implicit none

      integer, intent(in) :: sieve_size
      interface
         subroutine callback(arg)
            integer, intent(in) :: arg
         end subroutine callback
      end interface
      logical, dimension(:), allocatable :: sieve
      integer :: sqrt_sieve_size, i

      sqrt_sieve_size = floor(sqrt(real(sieve_size)))

      allocate(sieve(2:sieve_size))
      sieve(:) = .true.

      do i = 2, sieve_size
         if (sieve(i) .eqv. .true.) then
            if (i <= sqrt_sieve_size) sieve(i*i:sieve_size:i) = .false.

            call callback(i)
         end if
      end do

      deallocate(sieve)
   end subroutine each_prime

   subroutine assign_prime_if_factor(n)
      implicit none

      integer, intent(in) :: n

      if (mod(num, n) == 0) res = n
   end subroutine assign_prime_if_factor
end program ex003

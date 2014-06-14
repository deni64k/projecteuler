! Multiples of 3 and 5
! Problem 1
! If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
!
! Find the sum of all the multiples of 3 or 5 below 1000.

! Answer: 233168

program ex001
   implicit none

   integer, parameter :: num = 1000
   integer :: i, res = 0
  
   do i = 1, num - 1
      if (mod(i, 3) == 0 .OR. mod(i, 5) == 0) res = res + i
   end do

   print *, 'Answer:', res
end program ex001

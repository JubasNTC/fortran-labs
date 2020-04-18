program exercise_7_12_
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
   integer, allocatable    :: Z(:, :)
   integer                 :: s = 0

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (Z(N, M))
      read (In, *) (Z(i, :), i = 1, N)
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//N//'i0)') (Z(i, :), i = 1, N)
   close (Out)

   s = getMinSumColumn(Z, N, M)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a, T5, "= ", i0)') "Sum", s
   close (Out)

contains
   pure function getMinSumColumn(Z, N, M) result(s)
      integer             :: s, tempSum, minSum
      integer, intent(in) ::  Z(:, :), N, M
      integer             :: i, j
      
      minSum = 2147483646;
      s = 1

      do j = 1, M, 1
         tempSum = 0
         do i = 1, N, 1
            tempSum = tempSum + Z(i, j)
         end do

         if (tempSum < s) then
            minSum = tempSum
            s = j
         end if
      end do
   end function getMinSumColumn

end program exercise_7_12_
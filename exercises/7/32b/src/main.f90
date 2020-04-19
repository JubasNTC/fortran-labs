program exercise_32_b
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0, j = 0, k = 0, temp = 0
   integer, allocatable    :: Z(:, :), result(:, :)

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (Z(N, M))
      read (In, *) (Z(i, :), i = 1, N)
   close (In)
   
   result = Z

   do i = 1, N, 1
      do j = 1, M - 1, 1
         do k = 1, M - j - 1, 1
            if (result(i, k + 1) > result(i, k)) then
               temp = result(i, k)
               result(i, k) = result(i, k + 1)
               result(i, k + 1) = temp
            end if
         end do
      end do
   end do

   open (file=output_file, encoding=E_, newunit=Out)
      write(Out, *) "Source:"
      write (Out, '('//N//'i0)') (Z(i, :), i = 1, N)
      write(Out, *) "Sorted:"
      write (Out, '('//N//'i0)') (result(i, :), i = 1, N)
   close (Out)
end program exercise_32_b
program exercise_7_40
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0, j = 0, temp = 0
   integer, allocatable    :: matrix(:, :), result(:)

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (matrix(N, M))
      read (In, *) (matrix(i, :), i = 1, N)
   close (In)
   
   temp = N * M
   allocate(result(temp))
   temp = 0

   do i = 1, M, 1
      do j = 1, N, 1
         if (matrix(i, j) < 1 .and. mod(i + j, 2) == 0) then
            temp = temp + 1
            result(temp) = matrix(i, j)
         end if
      end do
   end do

   open (file=output_file, encoding=E_, newunit=Out)
      write(Out, *) "Result:"
      write (Out, "("//temp//"(i0, 1x))") result
   close (Out)
end program exercise_7_40
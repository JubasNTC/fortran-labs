program exercise_7_17_a
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0, j = 0
   integer, allocatable    :: Z(:, :)
   integer                 :: maxElement

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (Z(N, M))
      read (In, *) (Z(i, :), i = 1, N)
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//N//'i0)') (Z(i, :), i = 1, N)
   close (Out)

   maxElement = getMaxElement(Z, N, M)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a, T5, "= ", i0)') "Max", maxElement
      do i = 1, N, 1
         do j = 1, M, 1
            if (Z(i, j) == maxElement) then
               write (Out, *) i, j
            end if
         end do
      end do
   close (Out)

contains
   pure function getMaxElement(Z, N, M) result(maxElement)
      integer             :: maxElement
      integer, intent(in) ::  Z(:, :), N, M
      integer             :: i, j
      
      maxElement = -2147483646;

      do i = 1, N, 1
         do j = 1, M, 1
            if (Z(i, j) > maxElement) then
               maxElement = Z(i, j)
            end if
         end do
      end do
   end function getMaxElement

end program exercise_7_17_a
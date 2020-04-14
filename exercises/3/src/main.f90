program exercise_3
   use Environment

   implicit none
   character(*), parameter    :: array_file = "../data/input.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0, i = 0, size = 0
   real(R_), allocatable      :: array(:)
   real(R_)                   :: sum = 0
   character(:), allocatable  :: fmt

   open (file=array_file, newunit=In)
      read (In, *) size
      allocate (array(size))
      read (In, *) array
   close (In)

   do i = 1, size, 1
      sum = sum + array(i)**2
   end do

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '("Array:")')
      fmt ="("//size//"f6.2)"
      write (Out, fmt) array

      fmt = "(4(a, f0.2/))"
      write (Out, fmt) "result = ", sum
      
   close (Out)
end program exercise_3
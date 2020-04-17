program exercise_5_6
   use Environment

   implicit none
   character(*), parameter    :: array_file = "../data/input.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0, k, c, o, i, j, size, last
   integer, allocatable       :: array(:)
   character(:), allocatable  :: fmt

   open (file=array_file, newunit=In)
      read (In, *) k, o, i
      read (In, *) size
      allocate (array(size))
      read (In, *) array
   close (In)

   last = array(size)
   array(size) = k
   j = 1

   do while (array(j) /= k)
      j = j + 1
   end do

   array(size) = last

   if (j < size .or. array(size) == k) then
      c = o
   else 
      c = i
   end if

   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "("//size//"(i0, 1x))"
      write (Out, fmt) array
      fmt = "(4(a, i0/))"
      write (Out, fmt) "c = ", c
   close (Out)
end program exercise_5_6
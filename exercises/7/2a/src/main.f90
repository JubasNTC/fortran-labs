program exercise_7_2a
   use Environment

   implicit none
   character(*), parameter    :: array_file = "../data/input.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0, i, j, size, last_pos, temp
   integer, allocatable       :: array(:)
   character(:), allocatable  :: fmt

   open (file=array_file, newunit=In)
      read (In, *) size
      allocate (array(size))
      read (In, *) array
   close (In)

   last_pos = 1
   do i = 1, size, 1
      if (array(i) <= 0) then
         temp = array(i)
         array(i) = array(last_pos)
         array(last_pos) = temp
         last_pos = last_pos + 1
      end if
   end do

   do i = last_pos-1, 1, -1
      do j = 1, i
         if (array(j).gt.array(j+1)) then
            temp = array(j)
            array(j) = array(j+1)
            array(j+1) = temp
         end if
      end do
   end do

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '("Result:")')
      fmt = "("//size//"(i0, 1x))"
      write (Out, fmt) array
   close (Out)
end program exercise_7_2a
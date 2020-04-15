program exercise_2_21
   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0
   real(R_)                   :: a = 0, b = 0, c = 0, x = 0, delta = 0, r = 0, z = 0, result = 0
   character(:), allocatable  :: fmt

   fmt = "(4(a, f0.2))"

   open (file=input_file, newunit=In)
      read (In, *) a, b, c, x
   close (In)

   delta = 4 * a * c - b**2
   r = a + b * x + c * x**2
   z = 0.2 * c * x + b

   if (c > 0) then
      result = log(1 / sqrt(c)) * log(abs(2 * sqrt(c * r) + 2 * c * x + b))
   end if
   
   if (c < 0 .and. delta < 0) then
      result = -log(1 / sqrt(c)) * asin((2 * c * x + b) / sqrt(-delta))
   end if

   if (c > z .and. z > 0) then
      result = log(1 / sqrt(c)) * log(2 * x * c + b)
   end if

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, fmt) "a = ", a, " b = ", b, " c = ", c, " x = ", x
      write (Out, fmt) "delta = ", delta, " r = ", r, " z = ", z
      write (Out, fmt) "result = ", result
   close (Out)
end program exercise_2_21
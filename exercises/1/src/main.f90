program exercise_1
   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0
   real(R_)                   :: a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, x = 0, result = 0
   character(:), allocatable  :: fmt

   fmt = "(4(a, f0.2/))"

   open (file=input_file, newunit=In)
      read (In, *) a, b, c, d, e, f, x
   close (In)

   result = a * x**5 + b * x**4 + c * x**3 + d * x**2 + e * x + f
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, fmt) "a = ", a, "b = ", b, "c = ", c, "d = ", d, "e = ", e, "f = ", f, "x = ", x
      write (Out, fmt) "result = ", result
   close (Out)
end program exercise_1
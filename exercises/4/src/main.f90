function func(i) result(j)
   use Environment
   real(R_), intent(in) :: i
   real(R_)             :: j
   j = exponent(i) * sin(3.14 * i + 0.5)
end function func

program exercise_4_6_b

   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0
   real(R_)                   :: func, a = 0, b = 0, h = 0, gap = 0, i = 0, end = 0, result = 0
   character(:), allocatable  :: fmt

   open (file=input_file, newunit=In)
      read (In, *) a, b, h
   close (In)

   i = a + h
   end = b - h

   DO WHILE (i <= end)
      gap = gap + func(i)
      i = i + h
   END DO

   result = h * (func(a) / 2 + gap + func(b) / 2)

   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "(4(a, f0.1))"
      write (Out, fmt) "a = ", a, " b = ", b
      fmt = "(4(a, f0.4))"
      write (Out, fmt) "h = ", h
      write (Out, fmt) "I = ", result
   close (Out)
end program exercise_4_6_b
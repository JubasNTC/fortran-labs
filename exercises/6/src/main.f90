program exercise_6
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: sh_x = 0, x = 0

   open (file=input_file, newunit=In)
      read (In, *) x
   close (In)
   
   sh_x = shX(x)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(4(a, T16, "= ", e13.6/))') 'x', x, "sh(x)", sh_x
   close (Out)

contains
   real(R_) pure function shX(x)
      real(R_), intent(in) :: x
      
      real(R_), parameter :: PI_2 = 8 * Atan(1._R_)
      real(R_), parameter :: RELERR = epsilon(1._R_)
      real(R_) R(4), Numerators(4), Denominators(4), n_fact, x_s, x_8
      integer  Ns(8)
      x_s = Mod(x, PI_2)

      Numerators = x_s ** [3, 5, 7, 9]
      Numerators = Numerators * [-1, 1, -1, 1]
      
      x_8 = Numerators(4) / x_s
      
      Denominators = [2*3, 2*3*4*5, 2*3*4*5*6*7, 2*3*4*5*6*7*8*9]

      Ns = [2, 4, 6, 8, 3, 5, 7, 9]

      R = Numerators / Denominators

      shX = x_s + Sum(R)
      
      do while (Abs(R(4) / shX) >= RELERR)

         Numerators = Numerators * x_8

         n_fact = Denominators(4)

         Ns = Ns + 8
         
         Denominators = Ns(1:4) * Ns(5:8)

         Denominators(1) = n_fact * Denominators(1)
         Denominators(2) = Denominators(1) * Denominators(2)
         Denominators(3) = Denominators(2) * Denominators(3)
         Denominators(4) = Denominators(3) * Denominators(4)
         
         R = Numerators / Denominators

         shX = shX + Sum(R)
         if (Abs(R(4) / shX) < RELERR) exit
      end do
   end function shX
end program exercise_6
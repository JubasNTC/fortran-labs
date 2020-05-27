program reference_lab_2
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: F1, F2, F3
   integer                   :: first, last, k, In

   type(SourceLine), pointer :: InitialCode  => Null()   ! Первоначальный текст.
   !type(SourceLine), pointer :: ModdedCode   => Null()   ! Модифицированный текст.

   F1 = "../data/text.txt"
   F2 = "../data/params.txt"
   F3 = "output.txt"
   
   InitialCode => Read_Source_Code(F1)

   open (file=F2, newunit=In)
      read (In, *) first, last, k
   close (In)

   call Output_Source_Code(F3, InitialCode)

end program reference_lab_2

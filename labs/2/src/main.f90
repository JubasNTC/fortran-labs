program reference_lab_2
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: F1, F2, F3
   integer                   :: first, last, k, In

   type(SourceLine), pointer :: InitialCode  => Null()

   ! Файлы с входными и выходными данными
   F1 = "../data/text.txt"
   F2 = "../data/params.txt"
   F3 = "output.txt"
   
   ! Чтение входных данных
   InitialCode => Read_Source_Code(F1)

   ! Чтение входных данных
   open (file=F2, newunit=In)
      read (In, *) first, last, k
   close (In)

   ! Перестановка строк
   call Move_Strings(first, last, k, InitialCode)
   ! Печать результата
   call Output_Source_Code(F3, InitialCode)
end program reference_lab_2

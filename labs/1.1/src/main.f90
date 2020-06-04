program reference_lab_1_1
   use Environment

   implicit none
   integer, parameter                               :: EMPLOYEES_AMOUNT = 5, SURNAME_LEN = 15, POSITIONS_LEN = 15, MARKS_AMOUNT = 5
   character(:), allocatable                        :: input_file, output_file, format

   ! Массивы фамилий, должностей и временные
   character(SURNAME_LEN, kind=CH_)                 :: Surnames(EMPLOYEES_AMOUNT) = ""
   character(POSITIONS_LEN, kind=CH_)               :: Positions(EMPLOYEES_AMOUNT) = ""
   logical, allocatable                             :: Is_A_Technician(:), Is_A_Engineer(:)
   logical, allocatable                             :: Is_A_Senior_Engineer(:), Is_A_Lead_Engineer(:)

   integer :: In, Out, IO, i
   integer, parameter                               :: INDEXES(*) = [(i, i = 1, EMPLOYEES_AMOUNT)]
   integer                                          :: Technician_Amount = 0, Engineer_Amount = 0
   integer                                          :: Senior_Engineer_Amount = 0, Lead_Engineer_Amount = 0

   ! Файлы для чтения/записи входные и выходных данных.
   input_file = "../data/class.txt"
   output_file = "output.txt"

   ! Чтение списка класса: фамилии и должности.
   open (file=input_file, encoding=E_, newunit=In)
      format = '(2(a, 1x))'
      read (In, format, iostat=IO) (Surnames(i), Positions(i), i = 1, EMPLOYEES_AMOUNT)
   close (In)

   ! Удаление лишних пробелов
   do i = 1, EMPLOYEES_AMOUNT, 1
      Positions(i) = trim(Positions(i))
   end do

   ! Проверка ошибок при чтении вводных данных
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while reading class list."
      case(1:)
         write (Out, '(a)') "Error while reading class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while reading class list: ", io
   end select

   ! Печать вводных данных
   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(a)') "Исходный список:"
      write (Out, format, iostat=IO) (Surnames(i), trim(Positions(i)), i = 1, EMPLOYEES_AMOUNT)
   close (Out)

   ! Проверка по маске, ищем сколько соответсвий той или иной должности
   Is_A_Technician = Positions == CH__"technician"
   Is_A_Engineer = Positions == CH__"engineer"
   Is_A_Senior_Engineer = Positions == CH__"senior engineer"
   Is_A_Lead_Engineer = Positions == CH__"lead engineer"

   ! Считаем коливество по каждой должности
   Technician_Amount = Count(Is_A_Technician)
   Engineer_Amount = Count(Is_A_Engineer)
   Senior_Engineer_Amount = Count(Is_A_Senior_Engineer)
   Lead_Engineer_Amount = Count(Is_A_Lead_Engineer)

   ! Печатаем результат
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Результат:"
      format = "(4(a, i0))"
      write (Out, format, iostat=IO) "technician - ", Technician_Amount
      write (Out, format, iostat=IO) "engineer - ", Engineer_Amount
      write (Out, format, iostat=IO) "senior engineer - ", Senior_Engineer_Amount
      write (Out, format, iostat=IO) "lead engineer - ", Lead_Engineer_Amount
   close (Out)

end program reference_lab_1_1
program lab_1_5
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file

   integer                          :: IO, Out
   character(:), allocatable        :: format

   type(employee), pointer  :: Employees_List => Null(), Technician_List => Null(), Engineer_List => Null()
   type(employee), pointer  :: Senior_Engineer_List => Null(), Lead_Enginee_List => Null()
   integer(I_)              :: Technician_Amount = 0, Engineer_Amount = 0, Senior_Engineer_Amount = 0, Lead_Enginee_Amount = 0

   ! Файлы для чтения/записи входные и выходных данных.
   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   ! Чтение списка класса: фамилии и должности.
   Employees_List => Read_class_list(input_file)

   if (Associated(Employees_List)) then
      ! Печать вводных данных
      call Output_class_list(output_file, Employees_List, "Исходный список:", "rewind")

      ! Проверка по маске, ищем сколько соответсвий той или иной должности
      call Get_list_by_position(Employees_List, Technician_List, Technician_Amount, CH__"technician", 10)
      call Get_list_by_position(Employees_List, Engineer_List, Engineer_Amount, CH__"engineer", 8)
      call Get_list_by_position(Employees_List, Senior_Engineer_List, Senior_Engineer_Amount, CH__"senior engineer", 15)
      call Get_list_by_position(Employees_List, Lead_Enginee_List, Lead_Enginee_Amount, CH__"lead engineer", 13)

      ! Печатаем результат
      open (file=output_file, encoding=E_, position='append', newunit=Out)
         write (out, '(/a)') "Результат:"
         format = "(a, i0)"
         write (Out, format, iostat=IO) "technician - ", Technician_Amount
         write (Out, format, iostat=IO) "engineer - ", Engineer_Amount
         write (Out, format, iostat=IO) "senior engineer - ", Senior_Engineer_Amount
         write (Out, format, iostat=IO) "lead engineer - ", Lead_Enginee_Amount
      close (Out)
   end if

end program lab_1_5
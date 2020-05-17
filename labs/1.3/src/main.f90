program lab_1_3
   use Environment
   use Group_IO

   implicit none
   character(:), allocatable        :: input_file, output_file, data_file
   
   integer                          :: IO, Out
   character(:), allocatable        :: format

   type(employee)                   :: Group(EMPLOYEES_AMOUNT)
   type(employee), allocatable      :: Technician(:), Engineer(:), Senior_Engineer(:), Lead_Engineer(:)

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   
   Group = Read_class_list(data_file)

   call Output_class_list(output_file, Group, "Исходный список:", "rewind")

   Technician = Pack(Group, Group%Position == CH__"technician")
   Engineer = Pack(Group, Group%Position == CH__"engineer")
   Senior_Engineer = Pack(Group, Group%Position == CH__"senior engineer")
   Lead_Engineer = Pack(Group, Group%Position == CH__"lead engineer")

   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Результат:"
      format = "(a, i0)"
      write (Out, format, iostat=IO) "technician - ", size(Technician)
      write (Out, format, iostat=IO) "engineer - ", size(Engineer)
      write (Out, format, iostat=IO) "senior engineer - ", size(Senior_Engineer)
      write (Out, format, iostat=IO) "lead engineer - ", size(Lead_Engineer)
   close (Out)
end program lab_1_3
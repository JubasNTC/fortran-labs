module Group_IO
   use Environment

   implicit none
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: POSITIONS_LEN  = 15

   ! Структура данных для хранения данных о студенте.
   type employee
      character(SURNAME_LEN, kind=CH_)     :: Surname              = ""
      character(POSITIONS_LEN, kind=CH_)   :: Position             = ""
      type(employee), pointer              :: next                 => Null()
   end type employee

contains
   ! Чтение списка класса: фамилии, инициалы, полы и оценки.
   function Read_class_list(Input_File) result(Class_List)
      type(employee), pointer     :: Class_List
      character(*), intent(in)    :: Input_File
      integer  In

      open (file=Input_File, encoding=E_, newunit=In)
         Class_List => Read_employee(In)
      close (In)
   end function Read_class_list

   ! Чтение следующего студента.
   recursive function Read_employee(In) result(Emp)
      type(employee), pointer  :: Emp
      integer, intent(in)     :: In
      integer  IO
      character(:), allocatable  :: format
      
      allocate (Emp)
      format = '(2(a, 1x))'
      read (In, format, iostat=IO) Emp%Surname, Emp%Position
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
          Emp%next => Read_employee(In)
      else
         deallocate (Emp)
         nullify (Emp)
      end if
   end function Read_employee

   ! Вывод списка класса со средним баллом или без него.
   subroutine Output_class_list(Output_File, Class_List, List_Name, Position)
      character(*), intent(in)    :: Output_File, Position, List_Name
      type(employee), intent(in)  :: Class_List
      integer  :: Out
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_employee(Out, Class_List)
      close (Out)
   end subroutine Output_class_list

   recursive subroutine Output_employee(Out, Emp)
      integer, intent(in)        :: Out
      type(employee), intent(in) :: Emp
      
      integer                    :: IO
      character(:), allocatable  :: format

      format = '(2(a, 1x))'
      write (Out, format, iostat=IO) Emp%Surname, Emp%Position
      call Handle_IO_status(IO, "writing employee")
      if (Associated(Emp%next)) &
         call Output_employee(Out, Emp%next)
   end subroutine Output_employee
end module Group_IO 
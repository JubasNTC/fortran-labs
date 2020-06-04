program lab_1_2
   use Environment

   implicit none
   integer, parameter               :: EMPLOYEES_AMOUNT = 5, SURNAME_LEN = 15, POSITIONS_LEN = 15
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)
   character(:), allocatable        :: input_file, output_file

   ! Массивы фамилий, должностей и временные
   character(kind=CH_), dimension(POSITIONS_LEN) :: TECHNICIAN = "technician"
   character(kind=CH_), dimension(POSITIONS_LEN) :: LEAD_ENGINEER = "lead engineer"
   character(kind=CH_), dimension(POSITIONS_LEN) :: ENGINEER = "engineer"
   character(kind=CH_), dimension(POSITIONS_LEN) :: SENIOR_ENGINEER = "senior engineer"
   character(kind=CH_)                           :: Surnames(EMPLOYEES_AMOUNT, SURNAME_LEN)  = "", &
                                                    Positions(EMPLOYEES_AMOUNT, POSITIONS_LEN) = ""
   integer                                       :: Technician_Amount = 0, Engineer_Amount = 0
   integer                                       :: Senior_Engineer_Amount = 0, Lead_Engineer_Amount = 0
   integer                                       :: Out, IO
   character(:), allocatable                     :: fmt
   
   ! Файлы для чтения/записи входные и выходных данных.
   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   ! Чтение списка класса: фамилии и должности.
   call Read_class_list(input_file, Surnames, Positions)

   ! Печать вводных данных
   call Output_class_list(output_file, Surnames, Positions)

   ! Проверка по маске, ищем сколько соответсвий той или иной должности
   Technician_Amount = Get_amount_by_position(Positions, TECHNICIAN)
   Engineer_Amount = Get_amount_by_position(Positions, ENGINEER)
   Senior_Engineer_Amount = Get_amount_by_position(Positions, SENIOR_ENGINEER)
   Lead_Engineer_Amount = Get_amount_by_position(Positions, LEAD_ENGINEER)

   ! Печатаем результат
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Результат:"
      fmt = "(4(a, i0))"
      write (Out, fmt, iostat=IO) "technician - ", Technician_Amount
      write (Out, fmt, iostat=IO) "engineer - ", Engineer_Amount
      write (Out, fmt, iostat=IO) "senior engineer - ", Senior_Engineer_Amount
      write (Out, fmt, iostat=IO) "lead engineer - ", Lead_Engineer_Amount
   close (Out)

contains
   ! Функция чтения файла
   subroutine Read_class_list(Input_File, Surnames, Positions)
      character(*)         Input_File
      character(kind=CH_)  Surnames(:, :), Positions(:, :)
      intent (in)          Input_File
      intent (out)         Surnames, Positions

      integer In, IO, i
      character(:), allocatable  :: format
      
      ! Чтение
      open (file=Input_File, encoding=E_, newunit=In)
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // POSITIONS_LEN // 'a1, 1x)'
         read (In, format, iostat=IO) (Surnames(i, :), Positions(i, :), &
            i = 1, EMPLOYEES_AMOUNT)
         call Handle_IO_status(IO, "reading class list")
      close (In)
   end subroutine Read_class_list

   ! Функция печати
   subroutine Output_class_list(Output_File, Surnames, Positions)
      character(*)         Output_File
      character(kind=CH_)  Surnames(:, :), Positions(:, :)
      intent (in)          Output_File, Surnames, Positions

      integer                    :: Out, i, IO
      character(:), allocatable  :: format
   
      ! Печать
      open (file=output_file, encoding=E_, newunit=Out)
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // POSITIONS_LEN // 'a1, 1x)'
         write (Out, format, iostat=IO) &
            (Surnames(i, :), Positions(i, :), i = 1, EMPLOYEES_AMOUNT)
      close (Out)
   end subroutine Output_class_list

   ! Функция получения количества сотрудников по должности
   function Get_amount_by_position(Positions, Position) result(Position_Amount)
      integer                                                   :: Position_Amount, i
      character(kind=CH_), intent(in)                           :: Positions(EMPLOYEES_AMOUNT, POSITIONS_LEN)
      character(kind=CH_), dimension(POSITIONS_LEN), intent(in) :: Position

      Position_Amount  = 0

      ! Обходим массив должностей и ищем количество соответсвий
      do i = 1, EMPLOYEES_AMOUNT, 1
         if (count(Positions(i, :) == Position) == 1) then
            Position_Amount = Position_Amount + 1
         end if
      end do
   end function Get_amount_by_position
end program lab_1_2
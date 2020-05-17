program lab_1_2
   use Environment

   implicit none
   integer, parameter               :: EMPLOYEES_AMOUNT = 5, SURNAME_LEN = 15, POSITIONS_LEN = 15
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)
   character(:), allocatable        :: input_file, output_file

   character(POSITIONS_LEN, kind=CH_) :: TECHNICIAN = "technician"
   character(POSITIONS_LEN, kind=CH_) :: LEAD_ENGINEER = "lead engineer"
   character(POSITIONS_LEN, kind=CH_) :: ENGINEER = "engineer"
   character(POSITIONS_LEN, kind=CH_) :: SENIOR_ENGINEER = "senior engineer"
   character(kind=CH_)                :: Surnames(EMPLOYEES_AMOUNT, SURNAME_LEN)  = "", &
                                         Positions(EMPLOYEES_AMOUNT, POSITIONS_LEN) = ""
   integer                            :: Technician_Amount = 0, Engineer_Amount = 0
   integer                            :: Senior_Engineer_Amount = 0, Lead_Engineer_Amount = 0
   integer                            :: Out, IO
   character(:), allocatable          :: fmt
   

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   call Read_class_list(input_file, Surnames, Positions)

   call Output_class_list(output_file, Surnames, Positions)

   Technician_Amount = Get_amount_by_position(Positions, TECHNICIAN)
   Engineer_Amount = Get_amount_by_position(Positions, ENGINEER)
   Senior_Engineer_Amount = Get_amount_by_position(Positions, SENIOR_ENGINEER)
   Lead_Engineer_Amount = Get_amount_by_position(Positions, LEAD_ENGINEER)

   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Результат:"
      fmt = "(4(a, i0))"
      write (Out, fmt, iostat=IO) "technician - ", Technician_Amount
      write (Out, fmt, iostat=IO) "engineer - ", Engineer_Amount
      write (Out, fmt, iostat=IO) "senior engineer - ", Senior_Engineer_Amount
      write (Out, fmt, iostat=IO) "lead engineer - ", Lead_Engineer_Amount
   close (Out)

contains
   subroutine Read_class_list(Input_File, Surnames, Positions)
      character(*)         Input_File
      character(kind=CH_)  Surnames(:, :), Positions(:, :)
      intent (in)          Input_File
      intent (out)         Surnames, Positions

      integer In, IO, i
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // POSITIONS_LEN // 'a1, 1x)'
         read (In, format, iostat=IO) (Surnames(i, :), Positions(i, :), &
            i = 1, EMPLOYEES_AMOUNT)
         call Handle_IO_status(IO, "reading class list")
      close (In)
   end subroutine Read_class_list

   subroutine Output_class_list(Output_File, Surnames, Positions)
      character(*)         Output_File
      character(kind=CH_)  Surnames(:, :), Positions(:, :)
      intent (in)          Output_File, Surnames, Positions

      integer                    :: Out, i, IO
      character(:), allocatable  :: format
   
      open (file=output_file, encoding=E_, newunit=Out)
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // POSITIONS_LEN // 'a1, 1x)'
         write (Out, format, iostat=IO) &
            (Surnames(i, :), Positions(i, :), i = 1, EMPLOYEES_AMOUNT)
      close (Out)
   end subroutine Output_class_list

   function Get_amount_by_position(Positions, Position) result(Position_Amount)
      integer                                        :: Position_Amount, i
      character(kind=CH_), intent(in)                :: Positions(EMPLOYEES_AMOUNT, POSITIONS_LEN)
      character(kind=CH_)                            :: Tmp(1, POSITIONS_LEN)
      character(POSITIONS_LEN, kind=CH_), intent(in) :: Position

      Position_Amount  = 0
      Tmp = Position

      do i = 1, EMPLOYEES_AMOUNT, 1
         if (count(Positions(i, 1:15) == Tmp(1, 1:15)) > 0) then
            print *, "true"
            Position_Amount = Position_Amount + 1
         else
            print *, "false"
         end if
         print *, Positions(i, 1:15) == Tmp(1, 1:15)
    
      end do
   end function Get_amount_by_position
end program lab_1_2
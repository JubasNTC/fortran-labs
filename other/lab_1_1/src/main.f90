program lab_1_1
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 5, SURNAME_LEN = 15, MARKS_AMOUNT = 4
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_)

   character(:), allocatable  :: input_file, output_file, format

   ! Массивы фамилий, полов, оценок и средних оценов и временные
   ! переменные для обменов при сортировке.
   character(SURNAME_LEN, kind=CH_)                :: tmpSurname = "", Surnames(STUD_AMOUNT) = ""
   character(SURNAME_LEN, kind=CH_), allocatable   :: Boys_Surnames(:), Girls_Surnames(:)

   character(kind=CH_)                             :: Gender(STUD_AMOUNT) = ""
   
   integer                                         :: tmpMarks(MARKS_AMOUNT) = 0, Marks(STUD_AMOUNT, MARKS_AMOUNT) = 0
   integer, allocatable                            :: Boys_Marks(:, :), Girls_Marks(:, :), Boys_Pos(:), Girls_Pos(:)
   
   real(R_)                                        :: tmpAverMark = 0, Aver_Marks(STUD_AMOUNT) = 0
   real(R_), allocatable                           :: Boys_Aver_Marks(:), Girls_Aver_Marks(:)

   logical, allocatable                            :: Is_A_Boy(:), Is_A_Girl(:)
   integer                                         :: Boys_Amount = 0, Girls_Amount = 0

   integer :: In, Out, IO, i, j
   integer, parameter                              :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]
   logical :: Swap

   input_file = "../data/class.txt"
   output_file = "output.txt"
   ! Чтение списка класса: фамилии, инициалы, полы, оценки и средний.
   open (file=input_file, encoding=E_, newunit=In)
      format = '(2(a, 1x), ' // MARKS_AMOUNT // 'i1, f5.2)'
      read (In, format, iostat=IO) (Surnames(i), Gender(i), Marks(i, :), Aver_Marks(i), i = 1, STUD_AMOUNT)
   close (In)

   ! Обработка статуса чтения.
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

   ! Вывод списка класса.
   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(a)') "Исходный список:"
      ! Пояснения к записи те же, что и к чтению.
      write (Out, format, iostat=IO) (Surnames(i), Gender(i), Marks(i, :), Aver_Marks(i), i = 1, STUD_AMOUNT)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing class list."
      case(1:)
         write (Out, '(a)') "Error while writing class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing class list: ", io
   end select

   ! Составление логической маски, соответствующей юношам.
   Is_A_Boy       = Gender == MALE ! Gender == CH__"М" в некоторых компиляторах может пока не поддерживаться.
   Boys_Amount    = Count(Is_A_Boy)
   
   ! Получение массивов, связынных с юношами.
   ! 1-ый способ. Использование массива номеров юношей в списке.
   Boys_Pos   = Pack(INDEXES, Is_A_Boy)
   allocate (Boys_Surnames(Boys_Amount), &
      Boys_Marks(Boys_Amount, MARKS_AMOUNT))
   do concurrent (i = 1:Boys_Amount)
      ! Получение списков юношей.
      Boys_Surnames(i)  = Surnames(Boys_Pos(i))
      Boys_Marks(i, :)  = Marks(Boys_Pos(i), :)
   end do

   ! Вычисление средней оценки для юношей. Вне цикла для векторизации.
   Boys_Aver_Marks   = Real(Sum(Boys_Marks, dim=2), R_) / MARKS_AMOUNT

   Is_A_Girl      = .not. Is_A_Boy
   Girls_Amount   = STUD_AMOUNT - Boys_Amount
   
   ! Получение массивов, связынных с девушками.
   Girls_Pos   = Pack(INDEXES, Is_A_Girl)
   allocate (Girls_Surnames(Girls_Amount), &
      Girls_Marks(Girls_Amount, MARKS_AMOUNT))
   do concurrent (i = 1:Girls_Amount)
      ! Получение списков девушек.
      Girls_Surnames(i)  = Surnames(Girls_Pos(i))
      Girls_Marks(i, :)  = Marks(Girls_Pos(i), :)
   end do
      
   ! Вычисление средней оценки для девушек. Вне цикла для векторизации.
   Girls_Aver_Marks = Real(Sum(Girls_Marks, dim=2), R_) / MARKS_AMOUNT

   ! Сортировка списка юношей по среднему баллу методом пузырька.
   do i = Boys_amount, 2, -1
      ! Просматриваем список с начала, ставя в конец менее успешного.
      do j = 1, i-1
         Swap = .false.
         ! Проверка на то, стоит ли менять учащихся местами.
         if (Boys_Aver_Marks(j) < Boys_Aver_Marks(j+1)) then
            Swap = .true.
         else if (Boys_Aver_Marks(j) == Boys_Aver_Marks(j+1)) then
            if (Boys_Surnames(j) > Boys_Surnames(j+1)) then
               Swap = .true.
            else if (Boys_Surnames(j)==Boys_Surnames(j+1)) then
               Swap = .true.
            end if
         end if

         if (Swap) then
            tmpSurname           = Boys_Surnames(j+1)
            Boys_Surnames(j+1)   = Boys_Surnames(j)
            Boys_Surnames(j)     = tmpSurname

            tmpMarks             = Boys_Marks(j+1, :)
            Boys_Marks(j+1, :)   = Boys_Marks(j, :)
            Boys_Marks(j, :)     = tmpMarks

            tmpAverMark          = Boys_Aver_Marks(j+1)
            Boys_Aver_Marks(j+1) = Boys_Aver_Marks(j)
            Boys_Aver_Marks(j)   = tmpAverMark
         end if
      end do
   end do

   ! Сортировка списка девушек по среднему баллу методом пузырька.
   do i = Girls_Amount, 2, -1
      ! Просматриваем список с начала, ставя в конец менее успешного.
      do j = 1, i-1
         Swap = .false.
         ! Проверка на то, стоит ли менять учащихся местами.
         if (Girls_Aver_Marks(j) < Girls_Aver_Marks(j+1)) then
            Swap = .true.
         else if (Girls_Aver_Marks(j) == Girls_Aver_Marks(j+1)) then
            if (Girls_Surnames(j) > Girls_Surnames(j+1)) then
               Swap = .true.
            else if (Girls_Surnames(j)==Girls_Surnames(j+1)) then
               Swap = .true.
            end if
         end if

         if (Swap) then
            tmpSurname           = Girls_Surnames(j+1)
            Girls_Surnames(j+1)   = Girls_Surnames(j)
            Girls_Surnames(j)     = tmpSurname

            tmpMarks             = Girls_Marks(j+1, :)
            Girls_Marks(j+1, :)   = Girls_Marks(j, :)
            Girls_Marks(j, :)     = tmpMarks

            tmpAverMark          = Girls_Aver_Marks(j+1)
            Girls_Aver_Marks(j+1) = Girls_Aver_Marks(j)
            Girls_Aver_Marks(j)   = tmpAverMark
         end if
      end do
   end do

   ! Вывод отсортированного списка юношей со средним баллом.
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Успеваемость юношей:"
      write (Out, format, iostat=IO) &
         (Boys_Surnames(i), "М", Boys_Marks(i, :), Boys_Aver_Marks(i), i = 1, Boys_Amount)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted boys list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted boys list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted boys list: ", io
   end select
   
      ! Вывод отсортированного списка девушек со средним баллом.
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Успеваемость девушек:"
      write (Out, format, iostat=IO) (Girls_Surnames(i), "Ж", Girls_Marks(i, :), &
         Girls_Aver_Marks(i), i = 1, Girls_Amount)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted girls list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted girls list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted girls list: ", io
   end select

end program lab_1_1
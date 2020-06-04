module Group_Process
  use Environment
  use Group_IO

  implicit none

contains
   ! Получение списков по должности.
   pure recursive subroutine Get_list_by_position(Emp, List, Amount, Pos, L)
     type(employee), intent(in)          :: Emp
     type(employee), pointer             :: List
     integer(I_), intent(inout)          :: Amount
     integer, intent(in)                 :: L
     character(L, kind=CH_), intent(in)  :: Pos

     ! Если найден сотрудник нужной должности, то размещаем в новом списке элемент и копируем его данные.
     if (Emp%Position == Pos) then
        allocate (List, source=Emp)
        Amount = Amount + 1
        List%next => Null()
        ! Если ещё остались сотрудники, сканируем дальше, а в создаваемом списке передаём место СОСЕДА.
        if (Associated(Emp%next)) &
           call Get_list_by_position(Emp%next, List%next, Amount, Pos, L)
     ! Если ещё остались сотрудники, сканируем дальше, а в создаваемом списке передаём ПРЕЖНЕЕ место.
     else if (Associated(Emp%next)) then
        call Get_list_by_position(Emp%next, List, Amount, Pos, L)
     end if

  end subroutine Get_list_by_position

end module Group_process
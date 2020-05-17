module Group_Process
  use Environment
  use Group_IO

  implicit none

contains
   recursive subroutine Get_list_by_position(Emp, List, Amount, Pos, L)
     type(employee), intent(in)          :: Emp
     type(employee), pointer             :: List
     integer(I_), intent(inout)          :: Amount
     integer, intent(in)                 :: L
     character(L, kind=CH_), intent(in)  :: Pos
    
     print *, Emp%Position
     print *, Pos
     if (Emp%Position == Pos) then
        allocate (List, source=Emp)
        Amount = Amount + 1
        List%next => Null()
        if (Associated(Emp%next)) &
           call Get_list_by_position(Emp%next, List%next, Amount, Pos, L)
     else if (Associated(Emp%next)) then
        call Get_list_by_position(Emp%next, List, Amount, Pos, L)
     end if

  end subroutine Get_list_by_position

end module Group_process
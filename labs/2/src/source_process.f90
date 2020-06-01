module Source_Process
   use Environment
   use Source_IO

   implicit none

contains
   subroutine Move_Strings(NB, KB, K, InitialStrings)    
      type(SourceLine), pointer, intent(inout)  :: InitialStrings
      integer, intent (in)                   :: NB, KB, K

      type(SourceLine), pointer  :: CurrentInitial
      type(SourceLine), pointer  :: CurrentK, CurrentNB, PrevNB, CurrentKB

      integer                 ::  number_string = 1

      CurrentInitial => InitialStrings

      CurrentK  => null()
      CurrentNB => null()
      CurrentKB => null()

      do while (Associated(CurrentInitial))
         if (number_string == K) then
            CurrentK => CurrentInitial
         else if (number_string == NB - 1) then
            PrevNB => CurrentInitial
         else if (number_string == NB) then
            CurrentNB => CurrentInitial
         else if (number_string == KB) then
            CurrentKB => CurrentInitial
         end if
         CurrentInitial => CurrentInitial%Next
         number_string = number_string + 1
      end do

      if (NB == KB) then
         CurrentKB => CurrentNB
      end if

      if (NB == 1) then
         InitialStrings => CurrentKB%next
      else
         PrevNB%next => CurrentKB%next
      end if

      CurrentKB%next => CurrentK%next
      CurrentK%next => CurrentNB

   end subroutine Move_Strings
end module Source_process
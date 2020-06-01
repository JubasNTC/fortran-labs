module Source_Process
   use Environment
   use Source_IO

   implicit none

contains
   subroutine Move_Strings(NB, KB, K, InitialStrings)    
      type(SourceLine), pointer, intent(in)  :: InitialStrings
      integer, intent (in)                   :: NB, KB, K

      type(SourceLine), pointer  :: CurrentInitial, CurrentModded, CurrentTmp
      type(SourceLine), pointer  :: CurrentK, CurrentNB, CurrentKB

      integer                 ::  number_string = 1

      CurrentInitial => InitialStrings

      CurrentK  => null()
      CurrentNB => null()
      CurrentKB => null()

      ! Установка указателей на номера строк.
      do while (Associated(CurrentInitial))
         if (number_string == K) then
            CurrentK => CurrentInitial
         else if (number_string == NB-1) then
            CurrentNB => CurrentInitial
         else if (number_string == KB) then
            CurrentKB => CurrentInitial
         end if
         CurrentInitial => CurrentInitial%Next
         number_string = number_string + 1
      end do

      CurrentInitial => InitialStrings
      CurrentTmp     => CurrentInitial
      number_string = 1
      
      move_loop: &    
      do while (Associated(CurrentTmp))
         CurrentInitial => CurrentInitial%Next
         CurrentTmp     => CurrentTmp%Next 
         number_string = number_string + 1         
         if (number_string == K) then
            CurrentInitial => CurrentNB%next
         else if (number_string == NB-1) then
            CurrentInitial => CurrentKB%next
         else if (number_string == KB) then
            CurrentInitial => CurrentK%next
         end if
      end do move_loop

   end subroutine Move_Strings
end module Source_process

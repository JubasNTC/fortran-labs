program exercise_8_1
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, vectorSize = 0, i = 0
   integer, allocatable    :: matrix(:, :), tmatrix(:, :), vector(:), result(:)

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (matrix(N, M))
      read (In, *) (matrix(i, :), i = 1, N)
      read (In, *) vectorSize
      allocate (vector(vectorSize))
      read (In, *) vector
   close (In)
   
   tmatrix = transposeMatrix(matrix)

   result = matmul(tmatrix, vector)

   open (file=output_file, encoding=E_, newunit=Out)
      write(Out, *) "Matrix:"
      write (Out, '('//N//'i0)') (matrix(i, :), i = 1, N)
      write(Out, *) "Transpose matrix:"
      write (Out, '('//N//'i0)') (tmatrix(i, :), i = 1, N)
      write(Out, *) "Vector:"
      write (Out, "("//vectorSize//"(i0, 1x))") vector
      write(Out, *) "Result:"
      write (Out, "("//size(result)//"(i0, 1x))") result
   close (Out)

contains
   pure function transposeMatrix(matrix) result(result)
      integer             :: result(n, m)
      integer, intent(in) :: matrix(:, :)
      
      result = transpose(matrix)
   end function transposeMatrix
end program exercise_8_1
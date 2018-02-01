module bug1

  implicit none

  type s_t
    class(*), allocatable :: value
  end type s_t

  type v_t
    class(*), allocatable :: value(:)
  end type v_t

  type m_t
    class(*), allocatable :: value(:,:)
  end type m_t

contains
  type(s_t) function scalar(value)
    class(*), intent(in) :: value
    allocate(scalar%value, source=value)
  end function scalar

  type(v_t) function vector(value)
    class(*), intent(in) :: value(:)
    allocate(vector%value, source=value)
  end function vector

  type(m_t) function matrix(value)
    class(*), intent(in) :: value(:,:)
    allocate(matrix%value, source=value)
  end function matrix

end module bug1

program test_bug1
  use bug1
  implicit none

  type(s_t) :: f
  type(v_t) :: v
  type(m_t) :: m

  v = vector([1,2,3,4])
  m = matrix(reshape([1,2,3,4],[2,2]))

  f = scalar(v) !< when the class is a vectortor, no error
  f = scalar(m) !< when the class is a matrixrix, an error occurred
  !< Fortran runtime error:
  !< Array bound mismatrixch for dimension 1 of array '<<unknown>>' (4/2)

  !< Gfortran compile parameters contains: "-fcheck=all", remove it get no error.

end program test_bug1

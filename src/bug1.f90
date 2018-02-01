module bug1

  implicit none

  type foo_t
    class(*), allocatable :: value
  end type foo_t

  type vec_t
    class(*), allocatable :: value(:)
  end type vec_t

  type mat_t
    class(*), allocatable :: value(:,:)
  end type mat_t

contains
  type(foo_t) function foo(value)
    class(*), intent(in) :: value
    allocate(foo%value, source=value)
  end function foo

  type(vec_t) function vec(value)
    class(*), intent(in) :: value(:)
    allocate(vec%value, source=value)
  end function vec

  type(mat_t) function mat(value)
    class(*), intent(in) :: value(:,:)
    allocate(mat%value, source=value)
  end function mat

end module bug1

program test_bug1
  use bug1
  implicit none

  type(foo_t) :: f
  type(vec_t) :: v
  type(mat_t) :: m

  v = vec([1,2,3,4])
  m = mat(reshape([1,2,3,4],[2,2]))

  f = foo(v) !< when the class is a vector, no error
  f = foo(m) !< when the class is a matrix, an error occurred
  !< Fortran runtime error:
  !< Array bound mismatch for dimension 1 of array '<<unknown>>' (4/2)

  !< Gfortran compile parameters contains: "-fcheck=all", remove it get no error.

end program test_bug1

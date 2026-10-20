! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! @brief Handle errors and warnings

! This is the error-handling module for stand-alone Socrates.
! Host models will provide their own version of this module.
module socrates_ereport

implicit none

integer, parameter :: i_normal            = 0
!   Error free condition
integer, parameter :: i_err_fatal         = 1
!   Fatal error: immediate return
integer, parameter :: i_abort_calculation = 2
!   Calculation aborted
integer, parameter :: i_missing_data      = 3
!   Missing data error: conditional
integer, parameter :: i_err_io            = 4
!   I/O error
integer, parameter :: i_err_range         = 5
!   Interpolation range error
integer, parameter :: i_err_exist         = 6
!   Existence error
integer, parameter :: i_err_usage         = 7
!   Incorrect usage error
integer, parameter :: i_warning           = -1
!   Non-fatal warning

integer, parameter :: errormessagelength  = 512
!   Maximum length of error messages

contains

subroutine ereport(RoutineName, ErrorStatus, Cmessage)

use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
implicit none

character (len=*), intent(in) :: RoutineName
character (len=*), intent(in) :: Cmessage
integer, intent(inout)        :: ErrorStatus

if (ErrorStatus == i_err_usage) then
  write(output_unit,'(a,a)') 'Usage: ', Cmessage(1:len_trim(Cmessage))
  stop
else if (ErrorStatus > 0) then
  write(error_unit,'(a)')    '*** ERROR ********************************'
  write(error_unit,'(a,a)')  '* Error in routine: ', &
      RoutineName(1:len_trim(RoutineName))
  write(error_unit,'(a,i5)') '* Error Code: ', ErrorStatus
  write(error_unit,'(a,a)')  '* Error Message: ', &
      Cmessage(1:len_trim(Cmessage))
  write(error_unit,'(a)')    '******************************************'
  stop
else if (ErrorStatus < 0) then
  write(output_unit,'(a)')    '*** WARNING *****************************'
  write(output_unit,'(a,a)')  '* Warning in routine: ', &
      RoutineName(1:len_trim(RoutineName))
  write(output_unit,'(a,i5)') '* Warning Code: ', ErrorStatus
  write(output_unit,'(a,a)')  '* Warning Message: ', &
      Cmessage(1:len_trim(Cmessage))
  write(output_unit,'(a)')    '*****************************************'
end if

! Reset ErrorStatus
ErrorStatus = i_normal

end subroutine ereport
end module socrates_ereport

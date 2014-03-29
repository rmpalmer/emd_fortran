program testemd

  use emd_types
  use input
  use output
  use emd

  implicit none

  type(emd_parms) :: parms

  real, allocatable :: x(:)
  real, allocatable :: y(:)
  real, allocatable :: imfs(:,:)
  real, allocatable, target :: work(:,:)

  integer           :: n
  integer           :: istat
  integer           :: cnt

  integer           :: i

  character(len=128) :: command
  character(len=80)  :: arg
  logical            :: ok
  integer            :: argcnt
  integer            :: ierror
  integer            :: int_parm
  real               :: float_parm
  logical            :: logical_parm
  integer            :: loop_limit


  ok = .TRUE.
  loop_limit = 200

  call get_command_argument(0, command)
  argcnt = command_argument_count()

  i = 0
  do
     if (i .ge. argcnt) exit
     i = i + 1
     call get_command_argument(i, arg)
     select case (trim(arg))

        case ('-loop_limit')
           if (i .lt. argcnt) then
              i=i+1
              call get_command_argument(i,arg)
              read(arg,'(I8)',iostat=ierror) int_parm
              if (ierror .ne. 0) then
                 ok = .FALSE.
              else
                 loop_limit = int_parm
              end if
           else
              ok = .FALSE.
           endif

        case ('-max_imf')
           if (i .lt. argcnt) then
              i=i+1
              call get_command_argument(i,arg)
              read(arg,'(I8)',iostat=ierror) int_parm
              if (ierror .ne. 0) then
                 ok = .FALSE.
              else
                 parms%max_count = int_parm
              end if
           else
              ok = .FALSE.
           endif

        case ('-imf_thresh')
           if (i .lt. argcnt) then
              i=i+1
              call get_command_argument(i,arg)
              read(arg,'(F12.2)',iostat=ierror) float_parm
              if (ierror .ne. 0) then
                 ok = .FALSE.
              else
                 parms%imf_thresh = float_parm
              end if
           else
              ok = .FALSE.
           endif

        case ('-resid_thres')
           if (i .lt. argcnt) then
              i=i+1
              call get_command_argument(i,arg)
              read(arg,'(F12.2)',iostat=ierror) float_parm
              if (ierror .ne. 0) then
                 ok = .FALSE.
              else
                 parms%resid_thresh = float_parm
              end if
           else
              ok = .FALSE.
           endif

        case ('-peak_sens')
           if (i .lt. argcnt) then
              i=i+1
              call get_command_argument(i,arg)
              read(arg,'(F5.2)',iostat=ierror) float_parm
              if (ierror .ne. 0) then
                 ok = .FALSE.
              else
                 parms%peak_sens = float_parm
              end if
           else
              ok = .FALSE.
           endif

        case ('-ends_are_peaks')
           if (i .lt. argcnt) then
              i=i+1
              call get_command_argument(i,arg)
              read(arg,'(L)',iostat=ierror) logical_parm
              if (ierror .ne. 0) then
                 ok = .FALSE.
              else
                 parms%ends_are_peaks = logical_parm
              end if
           else
              ok = .FALSE.
           endif


        case ('-help')
           ok = .FALSE.
     end select
  end do

  if (.not. ok) then

     write(*,*) 'Usage: '

  else

     write(*,*) '------------------'
     write(*,*) 'Parameters'
     write(*,*) parms%max_count
     write(*,*) parms%imf_thresh
     write(*,*) parms%resid_thresh
     write(*,*) parms%ends_are_peaks
     write(*,*) parms%peak_sens
     write(*,*) loop_limit
     write(*,*) '------------------'

     call input_series(fname='data.csv', x=x, y=y, n=n)

     write(*,*) 'testemd',n

     allocate(imfs(n,parms%max_count),stat=istat)
     allocate(work(n,11), stat=istat)

     call decompose(data=y, n=n, imfs=imfs, cnt=cnt, parms=parms, work=work, loop_limit=loop_limit)

     call output_series(fname='final.csv', x=x, y=y, n=n, extra=imfs(:,1:cnt))

     deallocate(x, stat=ierror)
     deallocate(y, stat=ierror)

     deallocate(imfs, stat=ierror)
     deallocate(work, stat=ierror)


  end if


end program testemd

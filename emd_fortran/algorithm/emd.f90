module emd

  implicit none

  contains

    subroutine decompose(data, n, imfs, cnt, parms, work, loop_limit)

      use emd_types
      use peakdet
      use resamp
      use output

      implicit none

      type(emd_parms), intent(in)                :: parms
      integer, intent(in)                        :: n
      integer, intent(out)                       :: cnt
      real, intent(in),  dimension(n)            :: data
      real, intent(out), dimension(:,:)          :: imfs
      real, intent(out), dimension(n,10),target  :: work
      integer, intent(in), optional              :: loop_limit

      real, dimension(:),   pointer :: X
      real, dimension(:),   pointer :: h
      real, dimension(:),   pointer :: r
      real, dimension(:),   pointer :: mean
      real, dimension(:),   pointer :: min_vals
      real, dimension(:),   pointer :: min_locs
      real, dimension(:),   pointer :: max_vals
      real, dimension(:),   pointer :: max_locs
      real, dimension(:),   pointer :: reg_min
      real, dimension(:),   pointer :: reg_max
      real, dimension(:),   pointer :: diffs

      real         :: imf_limit
      real         :: resid_limit
      real         :: resid_range
      real         :: mean_range
      real         :: h_energy
      real         :: old_h_energy
      real         :: min_d
      real         :: max_d

      integer      :: mncnt
      integer      :: mxcnt

      integer      :: loop_cnt
      integer      :: debug_limit

      character(len=80)  :: fmt
      character(len=80) :: fname

      X        => work(:, 1)
      h        => work(:, 2)
      r        => work(:, 3)
      mean     => work(:, 4)
      min_vals => work(:, 5)
      min_locs => work(:, 6)
      max_vals => work(:, 7)
      max_locs => work(:, 8)
      reg_min  => work(:, 9)
      reg_max  => work(:,10)
      diffs    => work(:,11)

      cnt = 0

      resid_limit = parms%resid_thresh * (maxval(data)-minval(data))

      X = data
      r = X

      loop_cnt = 0
      if (present(loop_limit)) then
         debug_limit = loop_limit
      else
         debug_limit = 200
      end if

      h_energy = 0.0
      old_h_energy = 0.0
      fmt='(''debug_'',I0,''.csv'')'

      do

         ! should check here: if X is monotonic, then do not try to
         ! extract any (more) IMF's from it.

         diffs(1:n-1) = x(2:n) - x(1:n-1)
         min_d = minval(diffs(1:n-1))
         max_d = maxval(diffs(1:n-1))
         if ((min_d * max_d) .ge. 0) exit

         call peaks(data=X, n=n &
              ,min_vals=min_vals, min_locs=min_locs &
              ,max_vals=max_vals, max_locs=max_locs &
              ,mncnt=mncnt, mxcnt=mxcnt &
              ,sens=parms%peak_sens, ends_are_peaks=parms%ends_are_peaks)

         write (*,*) 'extrema',mncnt,mxcnt

         call resample(x=min_locs, y=min_vals, samcnt=mncnt, n=n, new=reg_min)
         call resample(x=max_locs, y=max_vals, samcnt=mxcnt, n=n, new=reg_max)

         !write (*,*) 'regularized min'
         !call output_series(x=r, y=reg_min, n=n)

         !write (*,*) 'regularized max'
         !call output_series(x=r, y=reg_max, n=n)

         mean = (reg_max + reg_min)/2.0

         !write(*,*) 'Mean'
         !call output_series(x=r, y=mean, n=n)

         h = X - mean
         !write (*,*) 'h'
         !call output_series(x=r, y=h, n=n)

         loop_cnt = loop_cnt + 1

         write(*,*) 'loop',loop_cnt

         write(fname,fmt) loop_cnt
         ! call output_columns(fname=fname, a=x, b=reg_min, c=reg_max, d=mean, e=h, n=n)

         if (loop_cnt .ge. debug_limit) exit


         !probably also need an 'energy not dropping' criterion to halt
         imf_limit = parms%imf_thresh * (maxval(h) - minval(h))
         mean_range = maxval(mean)-minval(mean)
         write(*,1000) 'IMF?',mean_range,imf_limit

         h_energy = dot_product(h, h)
         if ((abs(h_energy-old_h_energy)/h_energy .lt. 0.05) .or. &
              (mean_range .le. imf_limit)) then
            cnt = cnt + 1
            write(*,1010) cnt
            imfs(:,cnt) = h
            r = r - h
            X = r
            old_h_energy = 0.0
         else
            write(*,*) 'sifting'
            X = h
         endif

         old_h_energy = h_energy

         if (cnt .ge. parms%max_count) exit

         resid_range = maxval(r) - minval(r)
         write(*,1000) 'Stop?',resid_range,resid_limit
         if (resid_range .le. resid_limit) exit

      end do

1000  format (1x,A,1x,F5.2,1x,F5.2)
1010  format (1x,'Extracting IMF',1X,I6)

    end subroutine decompose

end module emd

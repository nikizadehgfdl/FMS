!***********************************************************************
!*                   GNU Lesser General Public License
!*
!* This file is part of the GFDL Flexible Modeling System (FMS).
!*
!* FMS is free software: you can redistribute it and/or modify it under
!* the terms of the GNU Lesser General Public License as published by
!* the Free Software Foundation, either version 3 of the License, or (at
!* your option) any later version.
!*
!* FMS is distributed in the hope that it will be useful, but WITHOUT
!* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!* for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with FMS.  If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************
program test   !test various aspects of mpp_mod

  use mpp_mod, only : mpp_init, mpp_exit, mpp_pe, mpp_npes, mpp_root_pe, stdout
  use mpp_mod, only : mpp_send, mpp_recv
  use mpp_mod, only : mpp_error, FATAL, WARNING, mpp_sync_self
  use mpp_io_mod, only : mpp_io_init
  use mpp_domains_mod, only: mpp_domain_is_tile_root_pe,mpp_get_domain_tile_root_pe
  use mpp_domains_mod, only: mpp_get_tile_pelist,mpp_get_tile_npes,mpp_get_io_domain,mpp_get_tile_id
  use mpp_domains_mod, only: mpp_define_domains,mpp_define_io_domain,mpp_define_layout,domain2d

  implicit none

  integer :: pe, npes, root,out_unit

  call mpp_init()
  call mpp_io_init()
  pe = mpp_pe()
  npes = mpp_npes()
  root = mpp_root_pe()
  out_unit = stdout()

  if( pe.EQ.root ) print *, '------------------> Calling test_send_recv <------------------'
    call test_send_recv(npes,pe,root,out_unit)
  if( pe.EQ.root ) print *, '------------------> Finished test_send_recv <------------------'
  if( pe.EQ.root ) print *, '------------------> Calling test_tile_send_recv <------------------'
    call test_tile_send_recv(npes,pe,root,out_unit)
  if( pe.EQ.root ) print *, '------------------> Finished test_tile_send_recv <------------------'
  call mpp_exit()

contains

  !***********************************************

  subroutine test_tile_send_recv(npes,pe,root,out_unit)
     integer, intent(in) :: npes,pe,root,out_unit
     type(domain2d) :: io_domain
     type(domain2d) :: domain
     integer :: io_tile_id(1), io_tile_root_pe
     integer, allocatable :: io_tile_pelist(:)
     logical :: is_io_tile_root_pe = .true.
     integer :: nx,ny,layout(2)
     integer :: nsent,nrcvd,np,from_pe
     integer, parameter :: COMM_TAG_11=111

     if(npes < 3)then
       call mpp_error(FATAL, "test_tile_send_recv: minimum of 3 ranks required. Not testing send_recv; too few ranks.")
     endif
     write(out_unit,*)
     nx=4320
     ny=1000
     call mpp_define_layout( (/1,nx,1,ny/), npes, layout )
     call mpp_define_domains( (/1,nx,1,ny/), layout, domain )
     call mpp_define_io_domain(domain, (/1,4/))
     io_domain = mpp_get_io_domain(domain)
     io_tile_id = mpp_get_tile_id(io_domain)
     is_io_tile_root_pe = mpp_domain_is_tile_root_pe(io_domain)
     io_tile_root_pe = mpp_get_domain_tile_root_pe(io_domain)
     np=mpp_get_tile_npes(io_domain)
     allocate(io_tile_pelist(np))
     call mpp_get_tile_pelist(io_domain,io_tile_pelist)
 
     !print*,'Comment me out!'

     nsent =0
     nrcvd =0
     if(is_io_tile_root_pe) then
       do np=2,size(io_tile_pelist) ! Note: np starts from 2 to exclude self      
          from_pe=io_tile_pelist(np)
          call mpp_recv(nrcvd, glen=1, from_pe=from_pe, tag=COMM_TAG_11)
          if (nrcvd .ne. 0) then
              print*,'test_tile_send_recv warning: pe ',mpp_pe(), ' got ',nrcvd, ' from pe ',from_pe
              call mpp_error(FATAL, "test_tile_send_recv: received junk")
          endif
       enddo
     else
       call mpp_send(nsent, plen=1, to_pe=io_tile_root_pe, tag=COMM_TAG_11)
     endif

  end subroutine test_tile_send_recv

  subroutine test_send_recv(npes,pe,root,out_unit)
     integer, intent(in) :: npes,pe,root,out_unit

     integer :: pelist(npes)
     integer :: i
     integer :: nx,ny,layout(2)
     integer :: nsent,nrcvd,np,from_pe
     integer, parameter :: COMM_TAG_11=111

     if(npes < 3)then
       call mpp_error(FATAL, "test_send_recv: minimum of 3 ranks required. Not testing send_recv; too few ranks.")
     endif
     write(out_unit,*)

     nx=4320
     ny=1000
     layout=(/0,0/)
     !print*,'layout=',layout
     do i=1,npes
       pelist(i) = i-1
     enddo

     nsent =0
     nrcvd =0
     if(mpp_pe() .eq. mpp_root_pe()) then
       do np=2,size(pelist) ! Note: np starts from 2 to exclude self      
          from_pe=pelist(np)
          call mpp_recv(nrcvd, glen=1, from_pe=from_pe, tag=COMM_TAG_11)
          if (nrcvd .ne. 0) then
              print*,'test_send_recv warning: pe ',mpp_pe(), ' got ',nrcvd, ' from pe ',from_pe
              call mpp_error(FATAL, "test_send_recv: received junk")
          endif
       enddo
     else
       call mpp_send(nsent, plen=1, to_pe=mpp_root_pe(), tag=COMM_TAG_11)
     endif

  end subroutine test_send_recv

end program test

* From jlanus@netscape.net Thu Jan 16 20:38:12 2003
* Date: Tue, 24 Sep 2002 12:20:07 -0400
* From: Juan Lanus <jlanus@netscape.net>
* To: ctags-users@lists.sourceforge.net
* Subject: [Ctags] seeking for help to set ctags to work with old FORTRAN
* 
* Hi
* 
* I'm trying to set ctags to work with some old FORTRAN 77 programs.
* I'm in Windows 2000, vim 6.1 and exuberant ctags 5.3.1.
* The programs are for VAX FORTRAN 77 as of 1988. Writen with tabs and 132 columns line length.
* 
* [...]
* 
* What am I doing wrong? This is my first contact with tags files of any sort and I'm vexed.
* 
* A sample .DES file is included at the bottom. 
* 
* TIA
* 
* Juan Lanus
* TECNOSOL
* Argentina
* 
* 
****************
*********************************************************************
*                                                                   *
* Sistema Personalizado - descripcion de registros - CGA - 1/12/89  *
*                                                                   *
* De tablas generales PERTAB012 - tabla 010                         *
*                                                                   *
*********************************************************************

        character*6     FEC010          !Fecha de Proceso aa/mm/dd
        character*5     URE010          !Ultimo Numero de Pedido
        character*5     REM010          !Ultimo Numero de Remito
        character*5     FAC010          !Ultimo Numero de Factura

        character*40    PER010          !Registro de Fecha

        equivalence     (PER010(1:1)  ,FEC010(1:1))
        equivalence     (PER010(7:7)  ,URE010(1:1))
        equivalence     (PER010(12:12),REM010(1:1))
        equivalence     (PER010(17:17),FAC010(1:1))

******************  Fin Registro de Fecha  ***********************************

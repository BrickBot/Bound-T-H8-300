-- ESF (spec)
--
-- This package provides only definition of the ESF file
-- as Ada.Text_IO.File_Type and procedures to create (open)
-- and close that file, and procedure to insert standard
-- warning or error message as an ESF comment into the
-- ESF file. A separate package is needed in order to avoid
-- elaboration circularity. 
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- This software is provided by the copyright holders and contributors "as is" and
-- any express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright owner or contributors be liable for
-- any direct, indirect, incidental, special, exemplary, or consequential damages
-- (including, but not limited to, procurement of substitute goods or services;
-- loss of use, data, or profits; or business interruption) however caused and
-- on any theory of liability, whether in contract, strict liability, or tort
-- (including negligence or otherwise) arising in any way out of the use of this
-- software, even if advised of the possibility of such damage.
--
-- Other modules (files) of this software composition should contain their
-- own copyright statements, which may have different copyright and usage
-- conditions. The above conditions apply to this file.
-------------------------------------------------------------------------------
--
-- $Revision: 1.5 $
-- $Date: 2015/10/24 20:05:24 $
--
-- $Log: esf.ads,v $
-- Revision 1.5  2015/10/24 20:05:24  niklas
-- Typo fix in description.
--
-- Revision 1.4  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-02-13 14:56:40  Niklas
-- BT-CH-0044.
--
-- Revision 1.2  2005/10/26 14:09:37  niklas
-- Added the function Is_Open.
--
-- Revision 1.1  2001/04/06 11:06:39  ville
-- First version
--


with Ada.Text_IO;


package ESF is


   File : Ada.Text_IO.File_Type;
   --
   -- The results of the HRT analysis are written into this file.


   function Is_Open return Boolean;
   --
   -- Whether the ESF file is open (has been Created and not yet Closed).


   procedure Create (File_Name : in String);
   --
   -- Creates (and opens) the ESF file.


   procedure Close;
   --
   -- Closes the ESF file.

   
   procedure Put_ESF_Comment (Text : in String);
   -- 
   -- Inserts the given Text into the ESF file if the file is open.

   
end ESF;

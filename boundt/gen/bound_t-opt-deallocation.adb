-- Bound_T.Opt.Deallocation (body)
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: bound_t-opt-deallocation.adb,v $
-- Revision 1.2  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.1  2011-08-31 04:17:12  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Assertions.Opt;  
with Bags_Opt;
with Flow.Computation.Opt;
with Flow.Const.Opt;
with Flow.Execution.Opt;
with Flow.Life.Opt;
with Flow.Opt;
with Flow.Origins.Opt;
with Flow.Slim.Opt;
with Hash_G_Opt;
with HRT.Opt;    
with OpenToken.Opt;
with Programs.Execution.Opt;
with Programs.Opt;
with Storage.Bounds.Opt;
with Storage.Cell_Numbering.Opt;
with Storage.Opt;
with String_Pool.Opt;
with Symbols.Opt;
with Tables_Opt;

with Decoder.Opt;


procedure Bound_T.Opt.Deallocation (Allow : in Boolean)
is
begin

   Assertions.Opt.Deallocate             := Allow;
   Bags_Opt.Deallocate                   := Allow;
   Flow.Computation.Opt.Deallocate       := Allow;
   Flow.Const.Opt.Deallocate             := Allow;
   Flow.Execution.Opt.Deallocate         := Allow;
   Flow.Life.Opt.Deallocate              := Allow;
   Flow.Opt.Deallocate                   := Allow;
   Flow.Origins.Opt.Deallocate           := Allow;
   Flow.Slim.Opt.Deallocate              := Allow;
   Hash_G_Opt.Deallocate                 := Allow;
   Standard.HRT.Opt.Deallocate           := Allow;
   OpenToken.Opt.Deallocate              := Allow;
   Programs.Execution.Opt.Deallocate     := Allow;
   Programs.Opt.Deallocate               := Allow;
   Storage.Bounds.Opt.Deallocate         := Allow;
   Storage.Cell_Numbering.Opt.Deallocate := Allow;
   Storage.Opt.Deallocate                := Allow;
   String_Pool.Opt.Deallocate            := Allow;
   Symbols.Opt.Deallocate_In_Stacks      := Allow;
   Tables_Opt.Deallocate                 := Allow;

   if not Allow then

      Decoder.Opt.Set_No_Deallocation;

   end if;

end Bound_T.Opt.Deallocation;

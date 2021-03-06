-- License.Size (decl)
--
-- Limit on the size of the program part for one analysis.
--
-- This version defines an unlimited size.
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
-- $Date: 2015/10/24 18:42:40 $
--
-- $Log: license-size.ads,v $
-- Revision 1.2  2015/10/24 18:42:40  niklas
-- Moved to free licence.
--
-- Revision 1.1  2015/10/24 18:14:58  niklas
-- First version.
--
-- Revision 1.2  2012-01-20 09:53:05  niklas
-- BT-CH-0223: Package License does not depend on package Flow.
--
-- Revision 1.1  2009-12-27 22:34:51  niklas
-- BT-CH-0205: Licensing based on code size and time/space dimensions.
--


package License.Size is


   Max_Measure_Default : constant Size_Measure_T := Size_Measure_T'Last;
   --
   -- Default value and upper bound on Max_Measure, below.


   Max_Measure : Size_Measure_T := Max_Measure_Default;
   --
   -- The maximum size measure in the combined call-graph
   -- of all root subprograms.


end License.Size;

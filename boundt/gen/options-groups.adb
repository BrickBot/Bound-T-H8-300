-- Options.Groups (body)
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:05:50 $
--
-- $Log: options-groups.adb,v $
-- Revision 1.4  2015/10/24 20:05:50  niklas
-- Moved to free licence.
--
-- Revision 1.3  2012-01-28 12:56:14  niklas
-- BT-CH-0226: Options update for AVR.
--
-- Revision 1.2  2011-09-01 13:04:38  niklas
-- Added the Timing group.
--
-- Revision 1.1  2011-08-31 04:17:13  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


package body Options.Groups is

begin

   Set_Group_Priority (Higher => Analysis    , Lower => Inputs);

   Set_Group_Priority (Higher => Analysis    , Lower => Assertions);

   Set_Group_Priority (Higher => Analysis    , Lower => Control_Flow);

   Set_Group_Priority (Higher => Analysis    , Lower => Timing);

   Set_Group_Priority (Higher => Analysis    , Lower => Stack_Usage);

   Set_Group_Priority (Higher => Timing      , Lower => Stack_Usage);

   Set_Group_Priority (Higher => Control_Flow, Lower => Loops);

   Set_Group_Priority (Higher => Control_Flow, Lower => Calls);

   Set_Group_Priority (Higher => Analysis    , Lower => Const_Prop);

   Set_Group_Priority (Higher => Analysis    , Lower => Arithmetic);

   Set_Group_Priority (Higher => Inputs      , Lower => Trace);

   Set_Group_Priority (Higher => Inputs      , Lower => Warn);

   Set_Group_Priority (Higher => Inputs      , Lower => Imp);

   Set_Group_Priority (Higher => Inputs      , Lower => Host_Memory);

   Set_Group_Priority (Higher => Inputs      , Lower => Resource_Limits);

end Options.Groups;

{
  This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.
   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
}

// Copyright (c) 2010 2011 - J. Aldo G. de Freitas Junior

{$MODE DELPHI}{$M+}{$H+}

Unit
	ActorConfig;

Interface

Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	Classes,
	SysUtils,
	Variants,
	Actors,
	LineParser;

// Available config options :
//
// setttl <value>
// start <classname> <instancename>
// addtarget <instancename> <targetname> 
// settarget <instancename> <targetname>
// deltarget <instancename> <targetname>
// config <instancename> <variable> <value>
// 

Implementation

End.
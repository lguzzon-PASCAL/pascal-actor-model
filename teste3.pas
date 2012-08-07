{
  This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.
   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
}

// Copyright (c) 2010 2011 2012 - J. Aldo G. de Freitas Junior

{$mode objfpc}
{$H+}{$M+}

Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	Classes,
	SysUtils,
	Actors,
	ActorMessages,
	ActorLogger,
	CustomActors,
	UDPActors;

Begin
	// Register messages
	ActorMessages.RegisterMessages;
	Actors.RegisterMessages;
	ActorLogger.RegisterMessages;
	CustomActors.RegisterMesssages;
	UDPActors.RegisterMessages;
	
	// Initialize systems
	Actors.Init('localhost', 'switchboard');
	ActorLogger.Init;
	CustomActors.Init;
	UDPActors.Init;
	
	// Start actors and set config
	StartActorInstance('TUDPReceiver', 'udpreceive1');
	StartActorInstance('TUDPSender', 'udpsender1');
	StartAUDPReceiver('udpreceiver1', 'udpsender1', '127.0.0.1', '4000', 1500);
	
	Repeat
		Write('Input something : '); ReadLn(gBuffer);
	Until gBuffer = 'quit';
	
	// Finish actors
	UDPActors.Fini;
	CustomActors.Fini;
	ActorLogger.Fini;
	Actors.Fini;
End.

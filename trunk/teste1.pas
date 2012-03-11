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
	CustomActors;

Type
	TScreenMessage = Class(TCustomStringActorMessage);

	TScreenWriterActor = Class(TActorThread)
	Public
		Procedure ScreenWrite(Var aMessage); Message 'tscreenmessage';
	End;

Procedure TScreenWriterActor.ScreenWrite(Var aMessage);
Var
	lMessage : TScreenMessage;
Begin
	lMessage := UnbundleMessage(aMessage) As TScreenMessage;
	WriteLn(ActorName, ': ', lMessage.Data);
End;

Var
	gBuffer : String;
	gScreenMessage : TScreenMessage;

Begin
	ActorMessageClassFactory.RegisterMessage(TScreenMessage);
	Actors.Init('localhost', 'switchboard');
	ActorLogger.Init;
	CustomActors.Init;
	RegisterActorClass(TScreenWriterActor);
	StartActorInstance('TScreenWriterActor', 'screen1');
	StartActorInstance('TScreenWriterActor', 'screen2');
	StartActorInstance('TScreenWriterActor', 'screen3');
	StartActorInstance('TLoadBalancerActor', 'screen');
	AddTargetToActor('screen', 'screen1');
	AddTargetToActor('screen', 'screen2');
	AddTargetToActor('screen', 'screen3');
	Repeat
		Write('Input something : '); ReadLn(gBuffer);
		If gBuffer <> 'quit' Then
		Begin
			gScreenMessage := TScreenMessage.Create('localhost', 'screen');
			gScreenMessage.Data := gBuffer;
			Switchboard.Mailbox.Push(gScreenMessage);
		End;
	Until gBuffer = 'quit';
	CustomActors.Fini;
	ActorLogger.Fini;
	Actors.Fini;
End.

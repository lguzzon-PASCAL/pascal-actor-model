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

Unit
	SerDesActors;

Interface

Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	RTTIObjects,
	Classes,
	Actors,
	ActorMessages,
	CustomActors,
	ActorLogger,
	SysUtils;

Type
	TSerializeActor = Class(TWithTargetActor)
	Public
		Procedure DefaultHandlerStr(Var aMessage); Override;
	End;

	TDeserializeActor = Class(TActorThread)
	Public
		Procedure StringToMessage(Var aMessage); Message 'TCustomStringActorMessage';
	End;

Implementation

// TSerializeActor

Procedure TSerializeActor.DefaultHandlerStr(Var aMessage);
Var
	lForward : TCustomStringActorMessage;
Begin
	lForward := TCustomStringActorMessage.Create(ActorName, Target);
	lForward.Data := Message.SaveRTTIToString;
	Send(lForward);
End;

// TDeserializeActor

Procedure TDeserializeActor.StringToMessage(Var aMessage);
Var
	lMessage : TCustomStringActorMessage;
	lMsgClass : String;
	lForward : TCustomActorMessage;
Begin
	lMessage := Message As TCustomStringActorMessage;
	lForward := ActorMessageClassFactory.Build(GetClassFromString(lMessage.Data));
	lForward.InitRTTIFromString(lMessage.Data);
	Send(lForward);
End;

End.
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
	ActorLogger;

Interface

Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	Classes,
	SysUtils,
	Variants,
	Actors,
	ActorMessages,
	EventLog;

Type
	TLoggerActor = Class(TActorThread)
	Private
		fLogger : TEventLog;
	Public
		Constructor Create(Const aName : String = ccDefaultLogger; CreateSuspended : Boolean = False; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure LogMessage(Var aMessage); Message 'tlogactormessage';
		Procedure WarningMessage(Var aMessage); Message 'twarningactormessage';
		Procedure ErrorMessage(Var aMessage); Message 'terroractormessage';
		Procedure DebugMessage(Var aMessage); Message 'tdebugactormessage';
		Procedure InfoMessage(Var aMessage); Message 'tinfoactormessage';
	End;

Procedure Init;
Procedure Fini;

Implementation

// TLoggerActor

Constructor TLoggerActor.Create(Const aName : String = ccDefaultLogger; CreateSuspended : Boolean = False; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, CreateSuspended, StackSize, aTimeout);
	fLogger := TEventLog.Create(Nil);
End;

Destructor TLoggerActor.Destroy;
Begin
	FreeAndNil(fLogger);
	Inherited Destroy;
End;

Procedure TLoggerActor.LogMessage(Var aMessage);
Var
	lMessage : TLogActorMessage;
Begin
	lMessage := UnbundleMessage(aMessage) As TLogActorMessage;
	fLogger.Log(lMessage.Data);
End;

Procedure TLoggerActor.WarningMessage(Var aMessage);
Var
	lMessage : TWarningActorMessage;
Begin
	lMessage := UnbundleMessage(aMessage) As TWarningActorMessage;
	fLogger.Warning(lMessage.Data);
End;

Procedure TLoggerActor.ErrorMessage(Var aMessage);
Var
	lMessage : TErrorActorMessage;
Begin
	lMessage := UnbundleMessage(aMessage) As TErrorActorMessage;
	fLogger.Error(lMessage.Data);
End;

Procedure TLoggerActor.DebugMessage(Var aMessage);
Var
	lMessage : TDebugActorMessage;
Begin
	lMessage := UnbundleMessage(aMessage) As TDebugActorMessage;
	fLogger.Debug(lMessage.Data);
End;

Procedure TLoggerActor.InfoMessage(Var aMessage);
Var
	lMessage : TInfoActorMessage;
Begin
	lMessage := UnbundleMessage(aMessage) As TInfoActorMessage;
	fLogger.Info(lMessage.Data);
End;

Procedure Init;
Begin
	RegisterActorClass(TLoggerActor);
	StartActorInstance('TLoggerActor', ccDefaultLogger);
End;

Procedure Fini;
Begin
End;

End.

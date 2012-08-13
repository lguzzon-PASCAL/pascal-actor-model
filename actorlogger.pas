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
		Constructor Create(Const aName : String = ccDefaultSwitchBoardName; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure LogMessage(Var aMessage); Message 'TLogActorMessage';
		Procedure WarningMessage(Var aMessage); Message 'TWarningActorMessage';
		Procedure ErrorMessage(Var aMessage); Message 'TErrorActorMessage';
		Procedure DebugMessage(Var aMessage); Message 'TDebugActorMessage';
		Procedure InfoMessage(Var aMessage); Message 'TInfoActorMessage';
	End;

Procedure Init;
Procedure Fini;
Procedure RegisterMessages;

Procedure ThrowLog(Const aMessage : String);
Procedure ThrowWarning(Const aMessage : String);
Procedure ThrowError(Const aMessage : String);
Procedure ThrowDebug(Const aMessage : String);
Procedure ThrowInfo(Const aMessage : String);

Implementation

// TLoggerActor

Constructor TLoggerActor.Create(Const aName : String = ccDefaultSwitchBoardName; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName);
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
	lMessage := Message As TLogActorMessage;
	fLogger.Log(lMessage.Data);
End;

Procedure TLoggerActor.WarningMessage(Var aMessage);
Var
	lMessage : TWarningActorMessage;
Begin
	lMessage := Message As TWarningActorMessage;
	fLogger.Warning(lMessage.Data);
End;

Procedure TLoggerActor.ErrorMessage(Var aMessage);
Var
	lMessage : TErrorActorMessage;
Begin
	lMessage := Message As TErrorActorMessage;
	fLogger.Error(lMessage.Data);
	{ Debug } WriteLn(lMessage.Data);
End;

Procedure TLoggerActor.DebugMessage(Var aMessage);
Var
	lMessage : TDebugActorMessage;
Begin
	lMessage := Message As TDebugActorMessage;
	fLogger.Debug(lMessage.Data);
End;

Procedure TLoggerActor.InfoMessage(Var aMessage);
Var
	lMessage : TInfoActorMessage;
Begin
	lMessage := Message As TInfoActorMessage;
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

Procedure RegisterMessages;
Begin
End;

Procedure ThrowLog(Const aMessage : String);
Var
	lLog : TLogActorMessage;
Begin
	lLog := TLogActorMessage.Create(MainThreadName, ccDefaultLogger);
	lLog.Data := aMessage;
	Switchboard.Mailbox.Push(lLog);
End;

Procedure ThrowWarning(Const aMessage : String);
Var
	lWarning : TWarningActorMessage;
Begin
	lWarning := TWarningActorMessage.Create(MainThreadName, ccDefaultLogger);
	lWarning.Data := aMessage;
	Switchboard.Mailbox.Push(lWarning);
End;

Procedure ThrowError(Const aMessage : String);
Var
	lError : TErrorActorMessage;
Begin
	lError := TErrorActorMessage.Create(MainThreadName, ccDefaultLogger);
	lError.Data := aMessage;
	Switchboard.Mailbox.Push(lError);
End;

Procedure ThrowDebug(Const aMessage : String);
Var
	lDebug : TDebugActorMessage;
Begin
	lDebug := TDebugActorMessage.Create(MainThreadName, ccDefaultLogger);
	lDebug.Data := aMessage;
	Switchboard.Mailbox.Push(lDebug);
End;

Procedure ThrowInfo(Const aMessage : String);
Var
	lInfo : TInfoActorMessage;
Begin
	lInfo := TInfoActorMessage.Create(MainThreadName, ccDefaultLogger);
	lInfo.Data := aMessage;
	Switchboard.Mailbox.Push(lInfo);
End;

End.
